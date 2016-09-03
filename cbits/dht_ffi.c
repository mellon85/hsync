/***** INCLUDES */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <unistd.h>
#include <pthread.h>

/* WIN32 compaibility layer  */
#ifdef _WIN32
#include <ws2tcpip.h>
#include <windows.h>
#else
#include <netinet/in.h>
#include <netdb.h>
#endif

#include "dht_ffi.h"

#include "dht/dht.c"

#ifdef __cplusplus
extern "C" {
#endif

/****** Constants */

#define BOOTSTRAP_SIZE 1024
#define BOOTSTRAP_BATCH_SIZE 16
#define DHT_ID_SIZE 20

#define DHT_ERROR_INIT 1
#define DHT_ERROR_SHUTDOWN 2
#define DHT_ERROR_BOOTSTRAP 3
#define DHT_ERROR_BOOTSTRAP_V4 4
#define DHT_ERROR_BOOTSTRAP_V6 5
#define DHT_ERROR_BOOTSTRAP_DUMP 6
#define DHT_ERROR_BOOTSTRAP_DUMP_V4 7
#define DHT_ERROR_BOOTSTRAP_DUMP_V6 8

/***** GLOBALS */

/* current DHT status */
static struct sockaddr_in* nodes_4 = NULL;
static struct sockaddr_in6* nodes_6 = NULL;
static int nodes_4_count;
static int nodes_6_count;
static int v4counter;
static int v6counter;

static int fd4;
static int fd6;
static int port;

static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

/***** Private interface */
static void ping_known_nodes();
static int save_bootstrap_nodes(const char* path);
static int load_bootstrap_nodes(const char* path);
static void close_fd(int *fd);

#define CHECK_MALLOC(typ, op, c, err) {if((op = (typ*)malloc(sizeof(typ)*c)) == NULL)\
    { rc = err; goto bad_bootstrap;}}
#define CHECK_FREAD(v,s,c,f,e) {if(fread(v,s,c,f) != c) \
    {fclose(f); rc = e; goto bad_bootstrap;} }
#define CHECK_FREE(p) {if(p){free(p); p = NULL;}}

static int load_bootstrap_nodes(const char* path)
{
    int rc = 0;

    if (f == NULL)
        return DHT_ERROR_BOOTSTRAP;

    debugf("read head\n");
    /* read header with all the counts */
    CHECK_FREAD(&nodes_4_count, sizeof(int), 1, f, DHT_ERROR_BOOTSTRAP);
    CHECK_FREAD(&nodes_6_count, sizeof(int), 1, f, DHT_ERROR_BOOTSTRAP);
    debugf("header %d %d\n", nodes_4_count, nodes_6_count);

    CHECK_MALLOC(struct sockaddr_in, nodes_4, nodes_4_count, DHT_ERROR_BOOTSTRAP_V4);
    CHECK_FREAD(nodes_4, sizeof(struct sockaddr_in), nodes_4_count, f, DHT_ERROR_BOOTSTRAP_V4);
    CHECK_MALLOC(struct sockaddr_in6, nodes_6, nodes_6_count, DHT_ERROR_BOOTSTRAP_V6);
    CHECK_FREAD(nodes_6, sizeof(struct sockaddr_in6), nodes_6_count, f, DHT_ERROR_BOOTSTRAP_V6);

    fclose(f);
    return rc;

bad_bootstrap:
    CHECK_FREE(nodes_4);
    CHECK_FREE(nodes_6);
    nodes_4_count = 0;
    nodes_6_count = 0;
    fclose(f);
    return rc;
}

#define CHECK_FWRITE(v,s,c,f,e) {if(fwrite(v,s,c,f) != c) \
    {fclose(f); free(tmp_path); return e;} }
static int save_bootstrap_nodes(const char* path)
{
    struct sockaddr_in sin[BOOTSTRAP_SIZE];
    struct sockaddr_in6 sin6[BOOTSTRAP_SIZE];

    int header[2] = {BOOTSTRAP_SIZE, BOOTSTRAP_SIZE};
    FILE *f;

    debugf("get nodes\n");
    dht_get_nodes(sin, &header[0], sin6, &header[1]);
    debugf("header %d %d\n", header[0], header[1]);

    debugf("open\n");
    char* tmp_path = strdup(path);
    tmp_path = strcat(tmp_path, ".tmp");
    f = fopen(tmp_path, "w+");
    if (f == NULL)
    {
        free(tmp_path);
        return DHT_ERROR_BOOTSTRAP_DUMP;
    }

    debugf("write head\n");
    CHECK_FWRITE(&header, sizeof(int), 2, f, DHT_ERROR_BOOTSTRAP_DUMP);

    debugf("write data\n");
    CHECK_FWRITE(&sin, sizeof(struct sockaddr_in), header[0], f, DHT_ERROR_BOOTSTRAP_DUMP_V4);
    CHECK_FWRITE(&sin6, sizeof(struct sockaddr_in6), header[1], f, DHT_ERROR_BOOTSTRAP_DUMP_V6);

    debugf("close file\n");
    fclose(f);

    /* swap file */
    debugf("rename\n");
    rename(tmp_path, path);
    free(tmp_path);

    return 0;
}

static void ping_known_nodes()
{
    /* Bootstrap nodes with ping messages in batches */
    for (int i = 0; i < BOOTSTRAP_BATCH_SIZE; ++i)
    {
        if (v4counter < nodes_4_count)
        {
            dht_ping_node((struct sockaddr*)&nodes_4[v4counter], sizeof(struct sockaddr_in));
            v4counter++;
        }
        if (v6counter < nodes_6_count)
        {
            dht_ping_node((struct sockaddr*)&nodes_6[v6counter], sizeof(struct sockaddr_in6));
            v6counter++;
        }
    }

    /* if we are done with the nodes we can free them and put to the pointers to
     * NULL */
    if (v4counter == nodes_4_count && nodes_4 != NULL)
    {
        free(nodes_4);
        nodes_4 = NULL;
        v4counter = 0;
        nodes_4_count = 0;
    }
    if (v6counter == nodes_6_count && nodes_6 != NULL)
    {
        free(nodes_6);
        nodes_6 = NULL;
        v6counter = 0;
        nodes_6_count = 0;
    }
}

static void close_fd(int *fd)
{
    if (fd != NULL)
    {
        if (*fd > 0)
            close(*fd);
        *fd = -1;
    }
}

/* functions needed by the DHT implementation */
int dht_blacklisted(const struct sockaddr *sa, int salen){
    (void) sa;
    (void) salen;
    return 0;
}

/* run_dht expects to be executed only once per application (due to the shared
 * state).  It expects up to 2 filedescriptors (ipv4 and ipv6). Use -1 if the
 * socket is not provided.  id must be a 20 random bytes identifying the node in
 * the DHT.
 * Return values:
 *  0 everything was fine
 *  1 dht initialization error
 *  2 dht shutdown error
 *
 * Doesn't return unless an error has to be returned or stop_dht is called from
 * another thread.
 */
int ffi_run_dht(
    int _fd4, /* ipv4 socket fd. -1 if not used */
    int _fd6, /* ipv6 socket fd. -1 it not used */
    int _port, /* port where the socket is bound */
    const unsigned char * restrict id /* 20 bytes id */,
    dht_callback callback,
    const char* restrict bootstrap_path /* bootstrap node data */)
{
    if (fd4 > 0 || fd6 > 0)
    {
        // Already initialized!
        return -2;
    }
    dht_debug = fopen("dht.log", "w");

    assert(_fd4 > 0 || _fd6 > 0);
    assert(id != NULL);
    assert(callback != NULL);
    assert(bootstrap_path != NULL);


    int rc = 0;
    fd4 = _fd4;
    fd6 = _fd6;
    time_t estimated_time = 1;
    v4counter = 0;
    v6counter = 0;
    port = htons(_port);

    /* Setup randomness for the dht */
    srand(time(NULL) ^ getpid() ^ (int)pthread_self());

    /*rc = */load_bootstrap_nodes(bootstrap_path);
    /*if (rc != 0)
        return rc;*/

    if (dht_init(fd4, fd6, id, NULL) < 0)
        return DHT_ERROR_INIT;

    pthread_mutex_lock(&lock);

    /* core loop */
    while(fd4 >= 0 || fd6 >= 0)
    {
        static char buff[4096];
        int len = 0;

        ping_known_nodes();

        struct timeval event_timeout;
        event_timeout.tv_sec = estimated_time;
        event_timeout.tv_usec = 1000; /* +1/10 of a second */

        // check events
        fd_set fd_read, fd_write, fd_error;
        FD_ZERO(&fd_read);
        FD_ZERO(&fd_write);
        FD_ZERO(&fd_error);

        if (fd4 >= 0) {
            FD_SET(fd4, &fd_read);
            FD_SET(fd4, &fd_error);
        }

        if (fd6 >= 0) {
            FD_SET(fd6, &fd_read);
            FD_SET(fd6, &fd_error);
        }

        pthread_mutex_unlock(&lock);
        int rc = select((fd4 > fd6 ? fd4 : fd6)+1, &fd_read, &fd_write, &fd_error, &event_timeout);
        pthread_mutex_lock(&lock);

        struct sockaddr_storage from;
        int fromlen = sizeof(from);
        if(rc > 0) {
            if(fd4 >= 0 && FD_ISSET(fd4, &fd_read))
                rc = recvfrom(fd4, buff, sizeof(buff) - 1, 0,
                              (struct sockaddr*)&from, &fromlen);
            else if(fd6 >= 0 && FD_ISSET(fd6, &fd_read))
                rc = recvfrom(fd6, buff, sizeof(buff) - 1, 0,
                              (struct sockaddr*)&from, &fromlen);
            debugf("received %d\n", rc);
        }
        if(rc > 0) {
            buff[rc] = '\0';
            dht_periodic(buff, rc, (struct sockaddr*)&from, fromlen,
                              &estimated_time, callback, NULL);
        }
    }
    pthread_mutex_unlock(&lock);

    debugf("all socket closed\n");
    debugf("bootstrap file saving%s\n", bootstrap_path);
    rc = save_bootstrap_nodes(bootstrap_path);
    if (rc != 0)
        return rc;
    debugf("bootstrap file saved %s\n", bootstrap_path);

    if (dht_uninit() < 0)
        return DHT_ERROR_SHUTDOWN;

    return 0;
}

void ffi_get_nodes(int *v4, int *v6) {
    assert(v4 != NULL || v6 != NULL); // one of the two should be not NULL
    pthread_mutex_lock(&lock);
    if (v4)
        if (fd4)
            *v4 = dht_nodes(AF_INET, NULL, NULL, NULL, NULL);
        else
            *v4 = 0;
    if (v6)
        if (fd6)
            *v6 = dht_nodes(AF_INET6, NULL, NULL, NULL, NULL);
        else
            *v4 = 0;
    if (dht_debug)
        dht_dump_tables(stdout);
    pthread_mutex_unlock(&lock);
}

void ffi_stop_dht() {
    pthread_mutex_lock(&lock);
    debugf("stop\n");
    if (fd4 > 0)
        close_fd(&fd4);
    if (fd6 > 0)
        close_fd(&fd6);
    fd4 = -1;
    fd6 = -1;
    pthread_mutex_unlock(&lock);
}

void ffi_search(const unsigned char* restrict id)
{
    assert(id != NULL);
    pthread_mutex_lock(&lock);

    if (fd4 >= 0)
        dht_search(id, port, AF_INET, NULL, NULL);
    if (fd6 >= 0)
        dht_search(id, port, AF_INET6, NULL, NULL);

    pthread_mutex_unlock(&lock);
}

void ffi_add_node_4(const void* restrict addr, short port)
{
    struct sockaddr_in in;
    memset(&in, 0, sizeof(struct sockaddr_in));
    in.sin_family = AF_INET;
    in.sin_port = htons(port);
    memcpy(&in.sin_addr, addr, sizeof(in.sin_addr));
    pthread_mutex_lock(&lock);
    dht_ping_node(
            (struct sockaddr*)&in,
            sizeof(struct sockaddr_in));
    pthread_mutex_unlock(&lock);
}

void ffi_add_node_6(const void* restrict addr, short port)
{
    struct sockaddr_in6 in;
    memset(&in, 0, sizeof(struct sockaddr_in6));
    in.sin6_family = AF_INET6;
    in.sin6_port = htons(port);
    memcpy(&in.sin6_addr, addr, sizeof(in.sin6_addr));
    pthread_mutex_lock(&lock);
    dht_ping_node(
            (struct sockaddr*)&in,
            sizeof(struct sockaddr_in6));
    pthread_mutex_unlock(&lock);
}

int dht_random_bytes(void *buf, size_t size)
{
    char *buf1 = (char*)buf;
    for(size_t i = 0; i < size; ++i)
        buf1[i] = rand();
    return 0;
}

#ifdef __cplusplus
}
#endif

