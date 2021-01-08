#include <stdio.h>
#include <errno.h>

#define register_errno(ERRNAME) real_reg(#ERRNAME "\0", ERRNAME)

void real_reg(const char* name, int value) {
    printf("(register-error %s %d)\n", name, value);
}

void print_header() {
    printf("(begin\n");
}

void print_trailer() {
    printf(")\n");
}
