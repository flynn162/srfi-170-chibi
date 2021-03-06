CHIBI := chibi-scheme
CHIBI_FFI := chibi-ffi
CC := cc
RMFV := rm -fv
TX := tools/txshout.sh

CFLAGS := -Wall -Werror -O3 $(CFLAGS)

uname := $(shell uname)
ifeq ($(uname), Darwin)
  so := dylib
else
  so := so
endif

all: 170/posix.$(so) 170.sld

170/posix.$(so): posix.stub posix.h after_init.h
	$(info ;; Compiling FFI stub...)
	$(TX) posix.tmp.c -- $(CHIBI_FFI) < $<
	$(CC) -fPIC -shared -lchibi-scheme $(CFLAGS) -o $@ posix.tmp.c

170.sld: tools/generate-sld.scm tools/GenerateErrorNames
	$(info ;; Generating sld...)
	$(TX) $@ -- $(CHIBI) $<

tools/GenerateErrorNames: tools/errnames.sh tools/errno_utils.h
	$(info ;; Compiling errno tool...)
	$(TX) $@.tmp.c -- $< -- $(CC)
	$(CC) $(CFLAGS) -o $@ $@.tmp.c

clean:
	$(RMFV) *.tmp *.tmp.c  tools/*.tmp tools/*.tmp.c
	$(RMFV) 170/*.so 170/*.dylib

dist-clean: clean
	$(RMFV) tools/GenerateErrorNames  170.sld

.PHONY: all clean dist-clean
