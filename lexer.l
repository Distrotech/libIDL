/***************************************************************************

    lexer.l (IDL flex scanner)

    Copyright (C) 1998 Andrew Veliath

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    $Id$

***************************************************************************/
%{
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "rename.h"
#include "util.h"
#include "IDL.h"
#include "parser.h"

#define tokreturn(token)	do {			\
	__IDL_prev_token_line = __IDL_cur_token_line;	\
	__IDL_cur_token_line = __IDL_cur_line;		\
	return token;					\
} while (0)

extern void		__IDL_do_pragma(const char *s);
#ifdef YYDEBUG
extern int		yydebug;
#endif
int			__IDL_prev_token_line;
int			__IDL_cur_token_line;
%}

whitespace		[ \t]*
newline			\n
cpp_pragma		^{whitespace}#{whitespace}pragma{whitespace}.*
cpp_status		^#.*
b8_int			0[0-9]*
b10_int			\-?[1-9][0-9]*
b16_int			0[xX][0-9A-Fa-f]+
float_lit		\-?[0-9]*\.[0-9]+([eE]\-?[0-9]+)?|\-?[0-9]+\.?([eE]\-?[0-9]+)?
fixed_lit		(\-?[0-9]*\.[0-9]+|\-?[0-9]+\.?[0-9]*)[dD]
happy_ident		[A-Za-z][A-Za-z0-9]*
err1_ident		_[A-Za-z0-9_]+
warn1_ident		[A-Za-z][A-Za-z0-9_]*
sqstring		\'[^\'\n]*[\'\n]
dqstring		\"[^\"\n]*[\"\n]

%%

{cpp_pragma}		{
	int n;
	char *p = yytext;
	char *s;

	while (isspace(*p) || *p == '#') ++p;
	s = p;
	sscanf(p, "%*6s%n", &n); s += n;
	while (isspace(*s)) ++s;
#ifdef YYDEBUG
	if (yydebug)
		printf("lexer.l: pragma '%s'\n", s);
#endif
	__IDL_do_pragma(s);
}
{cpp_status}		{
	char *filename = (char *)malloc(strlen(yytext) + 1);

	sscanf(yytext, "# %d %s", &__IDL_cur_line, filename);
	if (__IDL_cur_filename)
		free(__IDL_cur_filename);
	__IDL_cur_filename = strdup(filename + 1);
	__IDL_cur_filename[strlen(filename) - 2] = 0;
	free(filename);
	if (
#ifdef HAVE_CPP_PIPE_STDIN
	!strlen(__IDL_cur_filename)
#else
	!strcmp(__IDL_cur_filename, __IDL_tmp_filename)
#endif
	) {
		free(__IDL_cur_filename);
		__IDL_cur_filename = strdup(__IDL_real_filename);
	}
}
{whitespace}
{b8_int}		{
	sscanf(yytext, IDL_B8_FMT, &yylval.integer);
	tokreturn(TOK_INTEGER);
}
{b10_int}		{
	sscanf(yytext, IDL_UB10_FMT, &yylval.integer);
	tokreturn(TOK_INTEGER);
}
{b16_int}		{
	sscanf(yytext + 2, IDL_B16_FMT, &yylval.integer);
	tokreturn(TOK_INTEGER);
}
{fixed_lit}		{
	yylval.str = strdup(yytext);
	tokreturn(TOK_FIXEDP);
}
{float_lit}		{
	yylval.floatp = atof(yytext);
	tokreturn(TOK_FLOATP);
}
any			tokreturn(TOK_ANY);
attribute		tokreturn(TOK_ATTRIBUTE);
boolean			tokreturn(TOK_BOOLEAN);
case			tokreturn(TOK_CASE);
char			tokreturn(TOK_CHAR);
const			tokreturn(TOK_CONST);
context			tokreturn(TOK_CONTEXT);
default			tokreturn(TOK_DEFAULT);
double			tokreturn(TOK_DOUBLE);
enum			tokreturn(TOK_ENUM);
exception		tokreturn(TOK_EXCEPTION);
FALSE			tokreturn(TOK_FALSE);
fixed			tokreturn(TOK_FIXED);
float			tokreturn(TOK_FLOAT);
in			tokreturn(TOK_IN);
inout			tokreturn(TOK_INOUT);
interface		tokreturn(TOK_INTERFACE);
long			tokreturn(TOK_LONG);
module			tokreturn(TOK_MODULE);
native			tokreturn(TOK_NATIVE);
octet			tokreturn(TOK_OCTET);
oneway			tokreturn(TOK_ONEWAY);
out			tokreturn(TOK_OUT);
raises			tokreturn(TOK_RAISES);
readonly		tokreturn(TOK_READONLY);
sequence		tokreturn(TOK_SEQUENCE);
short			tokreturn(TOK_SHORT);
string			tokreturn(TOK_STRING);
struct			tokreturn(TOK_STRUCT);
switch			tokreturn(TOK_SWITCH);
TRUE			tokreturn(TOK_TRUE);
typedef			tokreturn(TOK_TYPEDEF);
unsigned		tokreturn(TOK_UNSIGNED);
union			tokreturn(TOK_UNION);
void			tokreturn(TOK_VOID);
wchar			tokreturn(TOK_WCHAR);
wstring			tokreturn(TOK_WSTRING);
::			tokreturn(TOK_OP_SCOPE);
\>\>			tokreturn(TOK_OP_SHR);
\<\<			tokreturn(TOK_OP_SHL);
{happy_ident}		{
	yylval.str = strdup(yytext);
	tokreturn(TOK_IDENT);
}
{err1_ident}		{
	yyerrorv("`%s' is not a valid identifier in IDL", yytext);
	yyerror("(Identifiers cannot start with an underscore)");
	yylval.str = strdup(yytext);
	tokreturn(TOK_IDENT);
}
{warn1_ident}		{
	yywarningv("`%s' underscores within identifiers are discouraged for use "
		   "with C-language IDL mappings", yytext);
	yylval.str = strdup(yytext);
	tokreturn(TOK_IDENT);
}
{sqstring}		{
	yylval.str = strdup(yytext + 1);
	yylval.str[strlen(yytext) - 2] = 0;
	tokreturn(TOK_SQSTRING);
}
{dqstring}		{
	yylval.str = strdup(yytext + 1);
	yylval.str[strlen(yytext) - 2] = 0;
	tokreturn(TOK_DQSTRING);
}
{newline}		{
	++__IDL_cur_line;
#ifdef YYDEBUG
	if (yydebug)
		printf("lexer.l: %s:%d:\n", 
		       __IDL_cur_filename, __IDL_cur_line);
#endif
}
.			tokreturn(yytext[0]);

%%

void __IDL_lex_init(void)
{
	__IDL_cur_line = 0;
	__IDL_cur_token_line = 0;
	__IDL_prev_token_line = 0;
	__IDL_cur_filename = NULL;
}

void __IDL_lex_cleanup(void)
{
	if (__IDL_cur_filename) {
		free(__IDL_cur_filename);
		__IDL_cur_filename = NULL;
	}
}

int yywrap(void)
{
	return 1;
}

/*
 * Local variables:
 * mode: C
 * c-basic-offset: 8
 * tab-width: 8
 * indent-tabs-mode: t
 * End:
 */
