/**************************************************************************

    lexer.l

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
	char *filename = (char *)malloc(strlen(yytext) + 1);

	sscanf(yytext, "# %d %s", &__idl_cur_line, filename);
	if (__idl_cur_filename)
		free(__idl_cur_filename);
	__idl_cur_filename = strdup(filename + 1);
	__idl_cur_filename[strlen(filename) - 2] = 0;
	free(filename);
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
	yylval.floatp = atof(yytext);
	return TOK_FIXEDP;
}
{float_lit}		{
	yylval.floatp = atof(yytext);
	return TOK_FLOATP;
}
any			{ return TOK_ANY; }
attribute		{ return TOK_ATTRIBUTE; }
boolean			{ return TOK_BOOLEAN; }
case			{ return TOK_CASE; }
char			{ return TOK_CHAR; }
const			{ return TOK_CONST; }
context			{ return TOK_CONTEXT; }
default			{ return TOK_DEFAULT; }
double			{ return TOK_DOUBLE; }
enum			{ return TOK_ENUM; }
exception		{ return TOK_EXCEPTION; }
FALSE			{ return TOK_FALSE; }
fixed			{ return TOK_FIXED; }
float			{ return TOK_FLOAT; }
in			{ return TOK_IN; }
inout			{ return TOK_INOUT; }
interface		{ return TOK_INTERFACE; }
long			{ return TOK_LONG; }
module			{ return TOK_MODULE; }
Object			{ return TOK_OBJECT; }
octet			{ return TOK_OCTET; }
oneway			{ return TOK_ONEWAY; }
out			{ return TOK_OUT; }
raises			{ return TOK_RAISES; }
readonly		{ return TOK_READONLY; }
sequence		{ return TOK_SEQUENCE; }
short			{ return TOK_SHORT; }
string			{ return TOK_STRING; }
struct			{ return TOK_STRUCT; }
switch			{ return TOK_SWITCH; }
TRUE			{ return TOK_TRUE; }
typedef			{ return TOK_TYPEDEF; }
unsigned		{ return TOK_UNSIGNED; }
union			{ return TOK_UNION; }
void			{ return TOK_VOID; }
wchar			{ return TOK_WCHAR; }
wstring			{ return TOK_WSTRING; }
::			{ return TOK_OP_SCOPE; }
\>\>			{ return TOK_OP_SHR; }
\<\<			{ return TOK_OP_SHL; }
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
	__idl_cur_line = 0;
	__idl_cur_filename = NULL;
}

void __idl_lex_cleanup(void)
{
	if (__idl_cur_filename) {
		free(__idl_cur_filename);
		__idl_cur_filename = NULL;
	}
}
