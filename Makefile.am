# -*- mode: makefile -*-
#
# libIDL Makefile.am
#
# Do not attempt a shared library (i.e. libtoolize) unless you know
# what you are doing (standard yacc/lex parsers are not reentrant).
#
# Copyright (C) 1998 Andrew Veliath <andrewtv@usa.net>
# Licensed under the GNU General Public License version 2
#
CFLAGS = -g -O2 -DYYDEBUG=1 -DIDL_LIBRARY -DLIBIDL_VERSION=@LIBIDL_VERSION@ -DCPP="\"@CPP@\""
YFLAGS = -d -v
#LFLAGS = @LFLAGS@
# we don't want the lex library
LIBS =

# rule to rename lex' yy prefix
lexer.c: lexer.l
	rm -f $@
	$(LEX) $(LFLAGS) $<
	sed -e 's/yy/__IDL_/g' $(LEX_OUTPUT_ROOT).c > $@
	rm -f $(LEX_OUTPUT_ROOT).c

EXTRA_DIST = BUGS
info_TEXINFOS = libIDL.texi
pkginclude_HEADERS = IDL.h
noinst_HEADERS = rename.h util.h parser.h
lib_LIBRARIES = libIDL.a

libIDL_a_SOURCES = parser.y lexer.l util.c
