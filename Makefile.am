# -*- mode: makefile -*-
#
# libIDL Makefile.am
#
CFLAGS			= -g -O2 -DYYDEBUG=1 -DIDL_LIBRARY @GLIB_INCLUDES@
YFLAGS			= -d -v
LFLAGS			= @LFLAGS@
LIBS			= -L. -lIDL @GLIB_LIBS@

# rule to rename lex' yy prefix
lexer.c: lexer.l
	rm -f $@
	$(LEX) $(LFLAGS) $<
	sed -e 's/yy/__IDL_/g' $(LEX_OUTPUT_ROOT).c > $@
	rm -f $(LEX_OUTPUT_ROOT).c

EXTRA_DIST		= BUGS
#info_TEXINFOS		= libIDL.texi
pkginclude_HEADERS	= IDL.h
lib_LIBRARIES		= libIDL.a
noinst_HEADERS		= rename.h util.h parser.h
noinst_PROGRAMS		= tstidl

libIDL_a_SOURCES	= parser.y lexer.l util.c
tstidl_SOURCES		= tstidl.c