# -*- mode: makefile -*-
#
# libIDL Makefile.am
#
INCLUDES		= -DYYDEBUG=1 -DYYERROR_VERBOSE=1 \
			  -DIDL_LIBRARY @GLIB_CFLAGS@ \
			  -DG_LOG_DOMAIN=\"libIDL\"
YFLAGS			= -d -v 2>/dev/null
LFLAGS			= @LFLAGS@

lexer.c: $(srcdir)/lexer.l parser.h
	@rm -f $@
	$(LEX) $(LFLAGS) -t $(srcdir)/lexer.l | sed -e 's/yy/__IDL_/g' >$@

stamp-parser: $(srcdir)/parser.y
	$(YACC) $(YFLAGS) $(srcdir)/parser.y
	touch $@

parser.c: stamp-parser
	test -f y.tab.c && mv -f y.tab.c parser.c || touch $@

parser.h: stamp-parser
	test -f y.tab.h && mv -f y.tab.h parser.h || touch $@

IDL.h: IDL.h.new
	@:

MOSTLYCLEANFILES	= parser.output y.output
DISTCLEANFILES		= IDL.h

EXTRA_DIST		= BUGS parser.y lexer.l \
			  Makefile.msc README.win32 \
			  libIDL.def \
			  stamp-parser

info_TEXINFOS		= libIDL2.texi
libIDLincludedir        = $(includedir)/libIDL-2.0/libIDL
libIDLinclude_HEADERS   = IDL.h
lib_LTLIBRARIES		= libIDL-2.la
noinst_PROGRAMS		= tstidl

bin_SCRIPTS		= libIDL-config-2

BUILT_SOURCES		= lexer.c parser.c parser.h IDL.h stamp-parser

libIDL_2_la_SOURCES	= parser.c parser.h lexer.c \
			  ns.c util.c util.h rename.h
libIDL_2_la_LDFLAGS	= -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)	\
			  -release $(LT_RELEASE)

tstidl_SOURCES		= tstidl.c
tstidl_LDADD		= libIDL-2.la @GLIB_LIBS@

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA= libIDL-2.0.pc

libIDL-config-2: libIDL-config
	cp -f libIDL-config libIDL-config-2
