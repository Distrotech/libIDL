#AUTOMAKE_OPTIONS = gnu
AUTOMAKE_OPTIONS = foreign
CFLAGS = -g -O2 -Wall -pedantic -DYYDEBUG=1
YFLAGS = -d -v
LFLAGS = -8

include_HEADERS = idl.h
lib_LIBRARIES = libIDL.a
noinst_PROGRAMS = tstidl

libIDL_a_SOURCES = parser.y lexer.l util.c

tstidl_LDFLAGS = -L.
tstidl_LDADD = -lIDL
tstidl_DEPENDENCIES = libIDL.a
