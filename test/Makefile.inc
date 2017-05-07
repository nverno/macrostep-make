# -*- mode: makefile -*-
ROOT         ?= .
NVP_INCL_DIR = ${ROOT}/include

NVP_CDEFS    = -DLINUX -D_GNU_SOURCE -D_DEFAULT_SOURCE
NVP_CFLAGS   = -std=c99 -g -Wall -Wextra \
		-I${NVP_INCL_DIR}        \
		${NVP_CDEFS}             \
		-Wno-sign-compare        \
		-Wno-unused-parameter

CC           = gcc
CFLAGS       = ${NVP_CFLAGS} ${NVP_CDEFS}
