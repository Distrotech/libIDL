# -*- mode: makefile -*-
#
# libIDL Makefile.am
#
INCLUDES		= -DYYDEBUG=1 -DYYERROR_VERBOSE=1 -DIDL_LIBRARY @GLIB_CFLAGS@
YFLAGS			= -d -v 2>/dev/null
LFLAGS			= @LFLAGS@
LIBS			= @GLIB_LIBS@

lexer.c: $(srcdir)/lexer.l
	@rm -f $@
	$(LEX) $(LFLAGS) -t $< | sed -e 's/yy/__IDL_/g' >$@

parser.c parser.h: $(srcdir)/parser.y
	$(YACC) $(YFLAGS) $<
	test -f y.tab.c && mv -f y.tab.c parser.c
	test -f y.tab.h && mv -f y.tab.h parser.h

MOSTLYCLEANFILES	= parser.output y.output
MAINTAINERCLEANFILES	= parser.c parser.h lexer.c
EXTRA_DIST		= BUGS parser.y lexer.l \
			  Makefile.msc README.win32 libIDL.def
info_TEXINFOS		= libIDL.texi
pkginclude_HEADERS	= IDL.h
lib_LTLIBRARIES		= libIDL.la
noinst_PROGRAMS		= tstidl

BUILT_SOURCES		= lexer.c parser.c parser.h

libIDL_la_SOURCES	= parser.c parser.h lexer.c ns.c util.c util.h rename.h
libIDL_la_LDFLAGS	= -version-info 5:4:5

tstidl_SOURCES		= tstidl.c
tstidl_LDFLAGS		= -static
tstidl_LDADD		= libIDL.la
