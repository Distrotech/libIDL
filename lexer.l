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
#include "IDL.h"
#include "rename.h"
#include "util.h"
#include "parser.h"

extern void		__IDL_do_pragma(const char *s);
#ifdef YYDEBUG
extern int		yydebug;
#endif
%}

whitespace		[ \t]*
newline			\n
cpp_pragma		^#{whitespace}pragma{whitespace}.*
cpp_status		^#.*
b8_int			0[0-9]*
b10_int			[1-9][0-9]*
b16_int			0[xX][0-9A-Fa-f]+
float_lit		\-?[0-9]*\.[0-9]+([eE]\-?[0-9]+)?|\-?[0-9]+\.?([eE]\-?[0-9]+)?
fixed_lit		(\-?[0-9]*\.[0-9]+|\-?[0-9]+\.?[0-9]*)[dD]
ident			[A-Za-z][A-Za-z0-9_]*
sqstring		\'[^\'\n]*[\'\n]
dqstring		\"[^\"\n]*[\"\n]

%option noyywrap

%%

{cpp_pragma}		{
	int n;
	char *s = yytext;

	sscanf(yytext, "#%*6s%n", &n); s += n;
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
}
{whitespace}
{b8_int}		{
	sscanf(yytext, "%lo", &yylval.integer);
	return TOK_INTEGER;
}
{b10_int}		{
	yylval.integer = atoi(yytext);
	return TOK_INTEGER;
}
{b16_int}		{
	sscanf(yytext + 2, "%lx", &yylval.integer);
	return TOK_INTEGER;
}
{fixed_lit}		{
	yylval.floatp = atof(yytext);
	return TOK_FIXEDP;
}
{float_lit}		{
	yylval.floatp = atof(yytext);
	return TOK_FLOATP;
}
any			return TOK_ANY;
attribute		return TOK_ATTRIBUTE;
boolean			return TOK_BOOLEAN;
case			return TOK_CASE;
char			return TOK_CHAR;
const			return TOK_CONST;
context			return TOK_CONTEXT;
default			return TOK_DEFAULT;
double			return TOK_DOUBLE;
enum			return TOK_ENUM;
exception		return TOK_EXCEPTION;
FALSE			return TOK_FALSE;
fixed			return TOK_FIXED;
float			return TOK_FLOAT;
in			return TOK_IN;
inout			return TOK_INOUT;
interface		return TOK_INTERFACE;
long			return TOK_LONG;
module			return TOK_MODULE;
Object			return TOK_OBJECT;
octet			return TOK_OCTET;
oneway			return TOK_ONEWAY;
out			return TOK_OUT;
raises			return TOK_RAISES;
readonly		return TOK_READONLY;
sequence		return TOK_SEQUENCE;
short			return TOK_SHORT;
string			return TOK_STRING;
struct			return TOK_STRUCT;
switch			return TOK_SWITCH;
TRUE			return TOK_TRUE;
typedef			return TOK_TYPEDEF;
unsigned		return TOK_UNSIGNED;
union			return TOK_UNION;
void			return TOK_VOID;
wchar			return TOK_WCHAR;
wstring			return TOK_WSTRING;
::			return TOK_OP_SCOPE;
\>\>			return TOK_OP_SHR;
\<\<			return TOK_OP_SHL;
{ident}			{
#ifdef YYDEBUG
	if (yydebug)
		printf("lexer.l: ident '%s'\n", yytext);
#endif
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
{newline}		{
	++__IDL_cur_line;
#ifdef YYDEBUG
	if (yydebug)
		printf("lexer.l: %s:%d:\n", 
		       __IDL_cur_filename, __IDL_cur_line);
#endif
}
.			return yytext[0];

%%

void __IDL_lex_init(void)
{
	__IDL_cur_line = 0;
	__IDL_cur_filename = NULL;
}

void __IDL_lex_cleanup(void)
{
	if (__IDL_cur_filename) {
		free(__IDL_cur_filename);
		__IDL_cur_filename = NULL;
	}
}

/*
 * Local variables:
 * mode: C
 * c-basic-offset: 8
 * tab-width: 8
 * indent-tabs-mode: t
 * End:
 */
