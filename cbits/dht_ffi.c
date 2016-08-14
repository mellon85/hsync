/***** INCLUDES */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

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

#include "dht.c"

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
static struct sockaddr_in* nodes_4;
static struct sockaddr_in6* nodes_6;
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
static time_t periodic_call(dht_callback /*callback*/);
static void wait_for_events_on_fd(int fd4, int fd6, time_t estimated_time);
static void close_fd(int *fd);

/****** INTERNAL */

static void wait_for_events_on_fd(int fd4, int fd6, time_t estimated_time)
{
    fd_set fd_read, fd_write, fd_error;
    FD_ZERO(&fd_read);
    FD_ZERO(&fd_write);
    FD_ZERO(&fd_error);

    int nfds = 0;
    if (fd4 >= 0)
    {
        ++nfds;
        FD_SET(fd4, &fd_read);
        FD_SET(fd4, &fd_error);
    }

    if (fd6 >= 0)
    {
        ++nfds;
        FD_SET(fd6, &fd_read);
        FD_SET(fd6, &fd_error);
    }

    struct timeval event_timeout;
    event_timeout.tv_sec = estimated_time;
    event_timeout.tv_usec = 1000; /* +1/10 of a second */

    pthread_mutex_unlock(&lock);
    select(nfds, &fd_read, &fd_write, &fd_error, &event_timeout);
    pthread_mutex_lock(&lock);
}

static time_t periodic_call(dht_callback callback)
{
    time_t tosleep;
    dht_periodic(NULL, 0, NULL, 0, &tosleep, callback, NULL);
    return tosleep;
}

#define CHECK_MALLOC(typ, op, c, err) {if((op = (typ)malloc(sizeof(typ)*c)) == NULL)\
    { rc = err; goto bad_bootstrap;}}
#define CHECK_FREAD(v,s,c,f,e) {if(fread(v,s,c,f) != s*c) \
    {fclose(f); rc = e; goto bad_bootstrap;} }
#define CHECK_FREE(p) {if(p!=NULL){free(p); p = NULL;}}

static int load_bootstrap_nodes(const char* path)
{
    FILE *f = fopen(path, "rb+");
    int rc = 0;

    if (f == NULL)
        return DHT_ERROR_BOOTSTRAP;

    /* read header with all the counts */
    CHECK_FREAD(&nodes_4_count, sizeof(int), 1, f, DHT_ERROR_BOOTSTRAP);
    CHECK_FREAD(&nodes_6_count, sizeof(int), 1, f, DHT_ERROR_BOOTSTRAP);

    CHECK_MALLOC(struct sockaddr_in*, nodes_4, nodes_4_count, DHT_ERROR_BOOTSTRAP_V4);
    CHECK_FREAD(&nodes_4, sizeof(struct sockaddr_in), nodes_4_count, f, DHT_ERROR_BOOTSTRAP_V4);
    CHECK_MALLOC(struct sockaddr_in6*, nodes_6, nodes_6_count, DHT_ERROR_BOOTSTRAP_V6);
    CHECK_FREAD(&nodes_6, sizeof(struct sockaddr_in6), nodes_6_count, f, DHT_ERROR_BOOTSTRAP_V6);

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

#define CHECK_FWRITE(v,s,c,f,e) {if(fwrite(v,s,c,f) != s*c) \
    {fclose(f); free(tmp_path); return e;} }
static int save_bootstrap_nodes(const char* path)
{
    struct sockaddr_in sin[BOOTSTRAP_SIZE];
    struct sockaddr_in6 sin6[BOOTSTRAP_SIZE];

    int sinc = BOOTSTRAP_SIZE;
    int sin6c = BOOTSTRAP_SIZE;
    FILE *f;

    dht_get_nodes(sin, &sinc, sin6, &sin6c);

    char* tmp_path = strdup(path);
    tmp_path = strcat(tmp_path, ".tmp");
    f = fopen(tmp_path, "w+");
    if (f == NULL)
    {
        free(tmp_path);
        return DHT_ERROR_BOOTSTRAP_DUMP;
    }

    CHECK_FWRITE(&sinc, sizeof(int), 1, f, DHT_ERROR_BOOTSTRAP_DUMP);
    CHECK_FWRITE(&sin6c, sizeof(int), 1, f, DHT_ERROR_BOOTSTRAP_DUMP);

    CHECK_FWRITE(&sin, sizeof(struct sockaddr_in), sinc, f, DHT_ERROR_BOOTSTRAP_DUMP_V4);
    CHECK_FWRITE(&sin6, sizeof(struct sockaddr_in6), sin6c, f, DHT_ERROR_BOOTSTRAP_DUMP_V6);
    fclose(f);

    /* swap file */
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
        *fd = -1
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
    assert(_fd4 > 0 || _fd6 > 0);
    assert(id != NULL && strlen(id) == 20);
    assert(callback != NULL);
    assert(bootstrap_path != NULL);

    int rc = 0;
    fd4 = _fd4;
    fd6 = _fd6;
    time_t estimated_time = 1;
    v4counter = 0;
    v6counter = 0;
    port = _port;

    /* Setup randomness for the dht */
    srand(time(NULL) ^ getpid() ^ (int)pthread_self());

    rc = load_bootstrap_nodes(bootstrap_path);
    if (rc != 0)
        return rc;

    if (dht_init(fd4, fd6, id, NULL) < 0)
        return DHT_ERROR_INIT;

    pthread_mutex_lock(&lock);

    /* core loop */
    while(fd4 >= 0 || fd6 >= 0)
    {
        ping_known_nodes();
        estimated_time = periodic_call(callback);

        /* wait for evenst will release the lock while the worker is suspended
         * in the select call. */
        wait_for_events_on_fd(fd4, fd6, estimated_time);
    }
    pthread_mutex_unlock(&lock);

    rc = save_bootstrap_nodes(bootstrap_path);
    if (rc != 0)
        return rc;

    if (dht_uninit() < 0)
        return DHT_ERROR_SHUTDOWN;

    return 0;
}

void ffi_get_nodes(int *v4, int *v6) {
    assert(v4 != NULL || v6 != NULL); // one of the two should be not NULL
    pthread_mutex_lock(&lock);
    if (v4)
        *v4 = dht_nodes(AF_INET, NULL, NULL, NULL, NULL);
    if (v6)
        *v6 = dht_nodes(AF_INET6, NULL, NULL, NULL, NULL);
    pthread_mutex_unlock(&lock);
}

void ffi_stop_dht() {
    pthread_mutex_lock(&lock);
    close_fd(&fd4);
    close_fd(&fd6);
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

void ffi_add_node(const struct sockaddr* restrict node, int len)
{
    assert(node != NULL);
    assert(len > 0);
    pthread_mutex_lock(&lock);
    dht_ping_node(node, len);
    pthread_mutex_unlock(&lock);
}

int dht_random_bytes(void *buf, size_t size)
{
    int *buf4 = (int*)buf;
    const int len4 = size % 4;
    for( int i = 0; i < len4; ++i )
    {
        buf4[i] = rand();
    }

    const int offset = len4*4;
    const int len1 = size - offset;
    char *buf1 = (char*)buf;
    for( int i = 0; i < len1; ++i )
    {
        buf1[i+offset] = rand();
    }

    return 0;
}

#ifdef __cplusplus
}
#endif

