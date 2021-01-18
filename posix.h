#include <chibi/eval.h>
#include <errno.h>
#include <string.h>

/* pa_nice */
#include <unistd.h>

/* pa_setenv pa_unsetenv */
#include <stdlib.h>

/* XSI strerror available if (_POSIX_C_SOURCE >= 200112L) && ! _GNU_SOURCE */
#if __linux__ && (_POSIX_C_SOURCE < 200112L || defined(_GNU_SOURCE))
    #define GNU_STRERROR 1
#endif

/* Use secure_getenv if provided */
#if defined(_GNU_SOURCE)
    #define GNU_SECURE_GETENV 1
#endif

/**
 * Ensure the `expression` is a string whose data can be unambiguously passed
 * from scheme to C.
 *
 * Returns a pointer to the string data on success, or NULL on failure.
 * DO NOT free the pointer!
 */
static const char* ensure_proper_string(sexp expression) {
    if (!sexp_stringp(expression)) return NULL;

    const char* data = sexp_string_data(expression);
    size_t size = sexp_string_size(expression);

    /* Check NULL termination */
    if (data[size] != '\0') return NULL;
    /* Check if there are NULLs in between */
    if (strlen(data) != size) return NULL;

    return data;
}

/**
 * Check if the expression can be passed to C as an unambiguous, safe string.
 *
 * Returns SEXP_TRUE on success and SEXP_FALSE on failure.
 */
static sexp pa_is_safe_c_string(sexp expression) {
    const char* result = ensure_proper_string(expression);
    return (result == NULL) ? SEXP_FALSE : SEXP_TRUE;
}

static int pa_errno() { return errno; }

/**
 * Fills the buffer with a NUL-terminated error message. The buffer should be
 * sufficiently large (e.g. >= 32 bytes) to hold the entire message.
 */
static void pa_strerror(int error_number, char* buf, const size_t len) {
    memset(buf, 0, len);

#if GNU_STRERROR == 1
    char* gnu_result = strerror_r(error_number, buf, len);
    if (gnu_result != buf) strncpy(buf, gnu_result, len - 1);
#else
    int old_errno = errno;
    int xsi_result = strerror_r(error_number, buf, len);
    if (xsi_result != 0) errno = old_errno;
#endif
}

sexp pa_strerror_stub(sexp ctx, sexp self, sexp_sint_t n,
                      sexp error_number) {
    /* Copy the error string into an on-stack buffer. */
    const size_t len = 255;
    char byte_buffer[len];
    pa_strerror(sexp_sint_value(error_number), byte_buffer, len);
    /* This function mem-copies the buffer */
    sexp res = sexp_c_string(ctx, byte_buffer, -1);
    /* Not a locally-scoped temp variable. Do not call preserve on it. */
    return res;
}

static int pa_nice(int delta) {
    errno = 0;
    return nice(delta);
}

static int pa_setenv(sexp key, sexp val) {
    const char* key_data = ensure_proper_string(key);
    const char* val_data = ensure_proper_string(val);
    if (key_data == NULL) return -2;
    if (val_data == NULL) return -3;
    return setenv(key_data, val_data, 1);
}

static int pa_unsetenv(sexp key) {
    const char* key_data = ensure_proper_string(key);
    if (key_data == NULL) return -2;
    return unsetenv(key_data);
}

/* feature flag generation */
#define XSTR(x) #x
#define check_feature(ctr, feature) \
    if(XSTR(feature)[0] == '1') { ctr++; fputs(#feature " ", stdout); }

static void pa_print_features() {
    size_t ctr = 0;
    check_feature(ctr, GNU_STRERROR);
    check_feature(ctr, GNU_SECURE_GETENV);
    if (ctr > 0) putchar('\n');
}
