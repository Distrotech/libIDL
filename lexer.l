%{
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "idl.h"
#include "rename.h"
#include "util.h"
#include "parser.h"

extern void		__idl_do_pragma(const char *s);

%}

whitespace		[ \t]*
newline			\n
cpp_pragma		^#[ \t]*pragma[ \t].*
cpp_status		^#.*
b8_int			0[0-9]*
b10_int			[1-9][0-9]*
b16_int			0[xX][0-9A-Fa-f]+
float_lit		\-?[0-9]*\.[0-9]+([eE]\-?[0-9]+)?|\-?[0-9]+\.?([eE]\-?[0-9]+)?
fixed_lit		(\-?[0-9]*\.[0-9]+|\-?[0-9]+\.?[0-9]*)[dD]
ident			[A-Za-z][A-Za-z0-9_]*
sqstring		\'[^\'\n]*[\'\n]
dqstring		\"[^\"\n]*[\"\n]

%%

{cpp_pragma}		{ __idl_do_pragma(yytext); }
{cpp_status}		{
	int line;
	char filename[512];

	sscanf(yytext, "# %d %511s", &__idl_cur_line, filename);
	if (__idl_cur_filename)
		free(__idl_cur_filename);
	__idl_cur_filename = strdup(filename + 1);
	__idl_cur_filename[strlen(filename) - 2] = 0;
}
{whitespace}
{b8_int}		{
	sscanf(yytext, "%o", &yylval.integer);
	return TOK_INTEGER;
}
{b10_int}		{
	yylval.integer = atoi(yytext);
	return TOK_INTEGER;
}
{b16_int}		{
	sscanf(yytext + 2, "%x", &yylval.integer);
	return TOK_INTEGER;
}
{fixed_lit}		{
	yylval.fixedp = atof(yytext);
	return TOK_FIXEDP;
}
{float_lit}		{
	yylval.floatp = atof(yytext);
	return TOK_FLOATP;
}
{ident}			{
	yylval.str = strdup(yytext);
	return TOK_IDENT;
}
{sqstring}		{
	yylval.str = strdup(yytext + 1);
	yylval.str[strlen(yytext) - 2] = 0;
	return TOK_SQSTRING;
}
{dqstring}		{
	yylval.str = strdup(yytext + 1);
	yylval.str[strlen(yytext) - 2] = 0;
	return TOK_DQSTRING;
}
{newline}		{ ++__idl_cur_line; }
.			{ return yytext[0]; }

%%

void __idl_lex_init(void)
{
	__idl_cur_line = __idl_nerrors = __idl_nwarnings = 0;
	__idl_cur_filename = NULL;
}

void __idl_lex_cleanup(void)
{
	if (__idl_cur_filename) {
		free(__idl_cur_filename);
		__idl_cur_filename = NULL;
	}
}
