# Test including Makefile.inc

ROOT = ..
include ${ROOT}/Makefile.inc

test: test.c 
	$(CC) ${NVP_CFLAGS} $(CFLAGS) -o $@ $^
