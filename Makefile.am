# -*- mode: makefile -*-
#
# libIDL Makefile.am
#
INCLUDES		= -DYYDEBUG=1 -DIDL_LIBRARY @GLIB_CFLAGS@
YFLAGS			= -d -v 2>/dev/null
LFLAGS			= @LFLAGS@
LIBS			= @GLIB_LIBS@

BUILT_SOURCES=lexer.c parser.c parser.h

# rule to rename lex' yy prefix
lexer.c: $(srcdir)/lexer.l
	rm -f $@
	$(LEX) $(LFLAGS) $< -o$@
	sed -e 's/yy/__IDL_/g' $(LEX_OUTPUT_ROOT).c > $@
	rm -f $(LEX_OUTPUT_ROOT).c

parser.c parser.h: $(srcdir)/parser.y
	rm -f $@
	$(YACC) $(YFLAGS) $< -o $@

EXTRA_DIST		= BUGS parser.y lexer.l
info_TEXINFOS		= libIDL.texi
pkginclude_HEADERS	= IDL.h
lib_LTLIBRARIES		= libIDL.la
noinst_PROGRAMS		= tstidl

libIDL_la_SOURCES	= parser.c parser.h lexer.c ns.c util.c util.h rename.h
libIDL_la_LDFLAGS	= -version-info 5:0:5

tstidl_SOURCES		= tstidl.c
tstidl_LDADD		= libIDL.la
