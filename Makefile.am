AUTOMAKE_OPTIONS = foreign
CFLAGS = -g -O2 -Wall -pedantic -DYYDEBUG=1
YFLAGS = -d -v
LEX_PREFIX = __IDL_
LEX_OUTPUT_ROOT = lex.$(LEX_PREFIX)
LFLAGS = -8 -P$(LEX_PREFIX)
LIBS =

include_HEADERS = idl.h
lib_LIBRARIES = libIDL.a
noinst_PROGRAMS = tstidl

libIDL_a_SOURCES = parser.y lexer.l util.c

tstidl_LDFLAGS = -L.
tstidl_LDADD = -lIDL
tstidl_DEPENDENCIES = libIDL.a
