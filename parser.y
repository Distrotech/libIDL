%{
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "idl.h"
#include "rename.h"
#include "util.h"

static IDL_tree *		IDL_tree_node_new(void);
static void			do_escapes(char *s);

IDL_tree			__idl_root;
static int			okay;

char *				__idl_cur_filename = NULL;
int				__idl_cur_line, __idl_nerrors, __idl_nwarnings;
%}

%union {
	IDL_tree tree;
	char *str;
	long integer;
	double floatp;
	double fixedp;
}

%token TOK_ANY TOK_ATTRIBUTE TOK_BOOLEAN TOK_CASE TOK_CHAR TOK_CONST
%token TOK_CONTEXT TOK_DEFAULT TOK_DOUBLE TOK_ENUM TOK_EXCEPTION
%token TOK_FALSE TOK_FIXED TOK_FLOAT TOK_IN TOK_INOUT TOK_INTERFACE
%token TOK_LONG TOK_MODULE TOK_OBJECT TOK_OCTET TOK_ONEWAY TOK_OUT
%token TOK_RAISES TOK_READONLY TOK_SEQUENCE TOK_SHORT TOK_STRING
%token TOK_STRUCT TOK_SWITCH TOK_TRUE TOK_TYPEDEF TOK_UNSIGNED
%token TOK_UNION TOK_VOID TOK_WCHAR TOK_WSTRING

%token<str> TOK_IDENT TOK_SQSTRING TOK_DQSTRING
%token<integer> TOK_INTEGER
%token<floatp> TOK_FLOATP
%token<fixedp> TOK_FIXEDP

%type<tree> idl_module
%type<str> string_lit dqstring

%%

start:			root_init
			idl_module
			root_finish			{
	if (okay)
		__idl_root = $2;
}
	;

root_init:						{
	__idl_root = NULL;
	okay = 1;
}
	;

root_finish:						{
}
	;

idl_module:		string_lit			{
	printf("%s\n", $1);
	$$ = NULL;
}
	;

string_lit:		dqstring
|			string_lit dqstring		{
	char *catstr = (char *)malloc(strlen($1) + strlen($2) + 1);
	strcpy(catstr, $1); free($1);
	strcat(catstr, $2); free($2);
	$$ = catstr;
}
	;

dqstring:		TOK_DQSTRING			{
	do_escapes($1);
	$$ = $1;
}
	;

%%

extern char *__idl_cur_filename;
extern int __idl_cur_line, __idl_nerrors, __idl_nwarnings;

void yyerrorl(const char *s, int ofs)
{
	++__idl_nerrors;
	fprintf(stderr, "%s:%d: error [%d], %s\n", 
		__idl_cur_filename, __idl_cur_line - 1 + ofs, __idl_nerrors, s);
}

void yywarningl(const char *s, int ofs)
{
	++__idl_nwarnings;
	fprintf(stderr, "%s:%d: warning [%d], %s\n",
		__idl_cur_filename, __idl_cur_line - 1 + ofs, __idl_nwarnings, s);
}

void yyerror(const char *s)
{
	yyerrorl(s, 0);
}

void yywarning(const char *s)
{
	yywarningl(s, 0);
}

void __idl_do_pragma(const char *s)
{
	yywarning("unknown pragma");
}

static IDL_tree IDL_tree_new(void)
{
	IDL_tree p;
}

static void do_escapes(char *s)
{
	char *os = s;

	assert(s != NULL);
	while (*s) {
		if ('\\' != *s++)
			continue;

		yywarning("unknown escape sequence");
	}
}
