#include <stdio.h>
#include <openssl/ssl.h>

#define DEBUG 0

#if defined(DEBUG) && DEBUG > 0
#define DEBUG_PRINT(fmt, args...) fprintf(stderr, "DEBUG: %s:%d:%s(): " fmt, \
                                          __FILE__, __LINE__, __func__, ##args)
#else
#define DEBUG_PRINT(fmt, args...) /* Don't do anything in release builds */
#endif

SSL_CTX *ssl_ctx;

SSL *ssl_wrap (int sock) {
  SSL_load_error_strings();
  SSL_library_init();
  ssl_ctx = SSL_CTX_new(SSLv23_client_method());
  SSL *conn = SSL_new(ssl_ctx);
  SSL_set_fd(conn, sock);
  int r = -1;
  while (r < 0) {
      r = SSL_connect(conn);
  }
  SSL_set_mode(conn, SSL_MODE_AUTO_RETRY);
  return conn;
}

int ssl_write (SSL *conn, const char *req, int length) {
  int w = SSL_write(conn, req, length);
  if (w < 0) {
    int ssl_error = SSL_get_error(conn, w);
    if (ssl_error == SSL_ERROR_WANT_WRITE) {
      DEBUG_PRINT("SSL_write wants write\n");
      return w;
    }

    if (ssl_error == SSL_ERROR_WANT_READ) {
      DEBUG_PRINT("SSL_write wants read\n");
      return w;
    }

    long error = ERR_get_error();
    const char* error_string = ERR_error_string(error, NULL);
    DEBUG_PRINT("could not SSL_write (returned -1): %s\n", error_string);
    return w;
  }
}

int ssl_read(SSL *conn, void* buf, int length) {
  int read = SSL_read(conn, buf, length);
  if (read < 0) {
    int ssl_error = SSL_get_error(conn, read);
    if (ssl_error == SSL_ERROR_WANT_WRITE) {
      DEBUG_PRINT("SSL_read wants write\n");
      return read;
    }

    if (ssl_error == SSL_ERROR_WANT_READ) {
      DEBUG_PRINT("SSL_read wants read\n");
      return read;
    }

    long error = ERR_get_error();
    const char* error_string = ERR_error_string(error, NULL);
    DEBUG_PRINT("could not SSL_read (returned -1) %s\n", error_string);
    return read;
  }
}

void ssl_close(SSL *conn) {
  SSL_shutdown(conn);
  SSL_free(conn);
  SSL_CTX_free(ssl_ctx);
}
