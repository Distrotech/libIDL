#ifndef __UTIL_H
#define __UTIL_H

#ifndef HAVE_STRDUP
#define strdup		__strdup
extern char *		strdup(const char *s);
#endif

extern void		yyerror(const char *s);
extern void		yyerrorl(const char *s, int ofs);
extern void		yywarning(const char *s);
extern void		yywarningl(const char *s, int ofs);

extern char *		__idl_cur_filename;
extern int		__idl_cur_line, __idl_nerrors, __idl_nwarnings;

#endif /* __UTIL_H */
