# -*- mode: makefile -*-
#
# libIDL Makefile.am
#
INCLUDES		= -DYYDEBUG=1 -DIDL_LIBRARY @GLIB_INCLUDES@
YFLAGS			= -d -v 2>/dev/null
LFLAGS			= @LFLAGS@
LIBS			= @GLIB_LIBS@

# rule to rename lex' yy prefix
lexer.c: lexer.l
	rm -f $@
	$(LEX) $(LFLAGS) $<
	sed -e 's/yy/__IDL_/g' $(LEX_OUTPUT_ROOT).c > $@
	rm -f $(LEX_OUTPUT_ROOT).c

EXTRA_DIST		= BUGS
#info_TEXINFOS		= libIDL.texi
pkginclude_HEADERS	= IDL.h
lib_LTLIBRARIES		= libIDL.la
noinst_PROGRAMS		= tstidl

libIDL_la_SOURCES	= parser.y lexer.l ns.c util.c util.h rename.h
libIDL_la_LDFLAGS	= -version-info 3:0:3

tstidl_SOURCES		= tstidl.c
tstidl_LDADD		= libIDL.la
