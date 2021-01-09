#include <errno.h>
#include <string.h>
#include <unistd.h>

/* (_POSIX_C_SOURCE >= 200112L) && ! _GNU_SOURCE */
#if _POSIX_C_SOURCE >= 200112L
    #ifndef _GNU_SOURCE
        #define XSI_STRERROR
    #endif
#endif

static int pa_errno() { return errno; }

static char* pa_strerror(int error_number) {
    const size_t len = 255;
    char* buf = malloc(len);
    if (buf == NULL) return NULL;
    memset(buf, 0, len);
    strcpy(buf, "(Unable to get the error message)");

#ifdef XSI_STRERROR
    int old_errno = errno;
    int xsi_result = strerror_r(error_number, buf, len);
    if (xsi_result != 0) errno = old_errno;
#else
    char* gnu_result = strerror_r(error_number, buf, len);
    if (gnu_result != buf) strncpy(buf, gnu_result, len - 1);
#endif

    return buf;
}

static int pa_nice(int delta) {
    errno = 0;
    return nice(delta);
}
