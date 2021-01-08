CHIBI := chibi-scheme
CHIBI_FFI := chibi-ffi
CC := cc
RMFV := rm -fv
TX := tools/txshout.sh

CFLAGS := -O3 $(CFLAGS)

export CC

all: 170/posix.so 170.sld

170/posix.so: posix.stub
	$(TX) posix.tmp.c -- $(CHIBI_FFI) < $<
	$(CC) -fPIC -shared -lchibi-scheme $(CFLAGS) -o $@ posix.tmp.c

170.sld: tools/generate-sld.scm tools/GenerateErrorNames
	$(TX) $@ -- $(CHIBI) $<

tools/GenerateErrorNames: tools/errnames.sh
	$(TX) $@.tmp.c -- $<
	$(CC) $(CFLAGS) -o $@ $@.tmp.c

clean:
	$(RMFV) *.tmp *.tmp.c  tools/*.tmp tools/*.tmp.c

dist-clean: clean
	$(RMFV) tools/GenerateErrorNames  170.sld

.PHONY: all clean dist-clean
