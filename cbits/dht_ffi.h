#include "dht/dht.h"

#ifdef __cplusplus
extern "C" {
#endif

struct sockaddr;

/***** FFI Interface */

/**
 * Starts the DHT node.
 * 
 * - fd4 ipv4 bound socket (-1 if not used)
 * - fd6 ipv6 bound socket (-1 if not used)
 * - port port listening on
 * - id the 20 bytes DHT node id
 * - callback the function to call back on various events
 * - bootstrap_path file path containing the bootstrap node informations
 * FFI import: safe
 */
int ffi_run_dht(int fd4, int fd6, const unsigned char* restrict id,
        dht_callback _callback);

/**
 * Sends to the DHT node a command to stop the execution and give control back
 * to the caller of ffi_run_dht.
 * FFI import: safe
 */
void ffi_stop_dht();

/**
 * executes a DHT search of a specific hash id and returns the data to the
 * dht_callback.
 * FFI import: safe
 */
void ffi_search(const unsigned char* restrict id, short port, dht_callback callback);

/**
 * Returns the count of ipv4 nodes known by the DHT node.
 * FFI import: safe
 */
void ffi_get_nodes(int* restrict /*v4*/, int* restrict /*v6*/);

/**
 * Tells the dht to ping the specific node hostname.
 * FFI import: safe
 */
void ffi_add_node(const struct sockaddr* restrict /*node*/, int /*len*/);

/**
 * Load bootstrap nodes from the saved file path
 * FFI import: safe
 */
int ffi_load_bootstrap_nodes(const char* path);

/**
 * Saves bootstrap nodes from the current state
 * FFI import: safe
 */
int ffi_save_bootstrap_nodes(const char* path);

#ifdef __cplusplus
}
#endif
