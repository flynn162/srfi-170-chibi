#include <errno.h>
#include <string.h>

static int pa_errno() { return errno; }

static char* pa_strerror(int error_number) {
    const size_t len = 255;
    char* buf = malloc(len);
    if (buf == NULL) return NULL;
    memset(buf, 0, len);
    strerror_r(error_number, buf, len);
    return buf;
}

static int pa_nice(int delta) {
    errno = 0;
    return nice(delta);
}
