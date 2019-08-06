# Test including Makefile.inc

ROOT = ..
include ${ROOT}/Makefile.inc

ee:
	@echo $(CPP)

test: test.c 
	$(CC) ${NVP_CFLAGS} $(CFLAGS) -o $@ $^

clean:
	$(RM) *~
