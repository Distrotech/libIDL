/**************************************************************************

    parser.y

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

static void			do_escapes(char *s);
static IDL_tree *		IDL_tree_node_new(void);

IDL_tree			__idl_root, __idl_symtab;
IDL_callback			__idl_cb = NULL;
static int			okay;

char *				__idl_cur_filename = NULL;
int				__idl_cur_line, __idl_nerrors, __idl_nwarnings;

void				__idl_print_tree(IDL_tree p);

static IDL_tree			list_start(IDL_tree a);
static IDL_tree			list_chain(IDL_tree a, IDL_tree b);
%}

%union {
	IDL_tree tree;
	char *str;
	long integer;
	float floatp;
	int boolean;
}

%token TOK_ANY			"any"
%token TOK_ATTRIBUTE		"attribute"
%token TOK_BOOLEAN		"boolean"
%token TOK_CASE			"case"
%token TOK_CHAR			"char"
%token TOK_CONST		"const"
%token TOK_CONTEXT		"context"
%token TOK_DEFAULT		"default"
%token TOK_DOUBLE		"double"
%token TOK_ENUM			"enum"
%token TOK_EXCEPTION		"exception"
%token TOK_FALSE		"FALSE"
%token TOK_FIXED		"fixed"
%token TOK_FLOAT		"float"
%token TOK_IN			"in"
%token TOK_INOUT		"inout"
%token TOK_INTERFACE		"interface"
%token TOK_LONG			"long"
%token TOK_MODULE		"module"
%token TOK_OBJECT		"Object"
%token TOK_OCTET		"octet"
%token TOK_ONEWAY		"oneway"
%token TOK_OUT			"out"
%token TOK_RAISES		"raises"
%token TOK_READONLY		"readonly"
%token TOK_SEQUENCE		"sequence"
%token TOK_SHORT		"short"
%token TOK_STRING		"string"
%token TOK_STRUCT		"struct"
%token TOK_SWITCH		"switch"
%token TOK_TRUE			"TRUE"
%token TOK_TYPEDEF		"typedef"
%token TOK_UNSIGNED		"unsigned"
%token TOK_UNION		"union"
%token TOK_VOID			"void"
%token TOK_WCHAR		"wchar"
%token TOK_WSTRING		"wstring"

%token TOK_OP_SCOPE		"::"
%token TOK_OP_SHR		">>"
%token TOK_OP_SHL		"<<"

%token <str>			TOK_IDENT TOK_SQSTRING TOK_DQSTRING
%token <integer>		TOK_INTEGER
%token <floatp>			TOK_FLOATP
%token <fixedp>			TOK_FIXEDP

%type <str>			sqstring dqstring dqstring_cat

%type <tree>			specification
%type <tree>			definition_list definition
%type <tree>			type_dcl type_spec type_declarator declarator_list declarator
%type <tree>			simple_type_spec constr_type_spec base_type_spec
%type <tree>			simple_declarator /* complex_declarator array_declarator */
%type <tree>			struct_type union_type
%type <tree>			member member_list enumerator_list
%type <tree>			switch_type_spec switch_body

%type <tree>			string_lit ident floating_pt_type integer_type
%type <tree>			char_type wide_char_type boolean_type octet_type
%type <tree>			any_type object_type enum_type scoped_name

%type <integer>			signed_int unsigned_int

%start start

%%

idl_init:						{
	okay = 1;
	__idl_symtab = NULL;
}
	;

start:			idl_init
			specification
			idl_finish			{
	__idl_root = NULL;
	if (okay)
		__idl_root = $2;
}
	;

idl_finish:						{
}
	;

specification:		definition_list
	;

definition_list:	definition			{ $$ = list_start($1); }
|			definition_list definition	{ $$ = list_chain($1, $2); }
	;

definition:		type_dcl ';'
	;

type_dcl:		"typedef" type_declarator	{ $$ = $2; }
	;

type_declarator:	type_spec declarator_list	{ $$ = IDL_type_dcl_new($1, $2); }
	;

type_spec:		simple_type_spec
|			constr_type_spec
	;

simple_type_spec:	base_type_spec
	;

constr_type_spec:	struct_type
|			union_type
	;

struct_type:		"struct" ident '{'
				member_list
			'}'				{ $$ = IDL_type_struct_new($2, $4); }
	;

union_type:		"union" ident "switch" '('
				switch_type_spec
			')' '{'
				switch_body
			'}'				{ $$ = IDL_type_union_new($2, $5, $8); }
	;

switch_type_spec:	integer_type
|			char_type
|			boolean_type
|			enum_type
|			scoped_name
	;

enum_type:		"enum" ident '{'
				enumerator_list
			'}'				{ $$ = IDL_type_enum_new($2, $4); }
	;

scoped_name:						{ $$ = NULL; }
	;

switch_body:						{ $$ = NULL; }
	;

enumerator_list:	ident				{ $$ = list_start($1); }
|			enumerator_list ident		{ $$ = list_chain($1, $2); }
	;

member_list:		member				{ $$ = list_start($1); }
|			member_list member		{ $$ = list_chain($1, $2); }
	;

member:			type_spec declarator_list ';'	{ $$ = IDL_member_new($1, $2); }
	;

base_type_spec:		floating_pt_type
|			integer_type
|			char_type
|			wide_char_type
|			boolean_type
|			octet_type
|			any_type
|			object_type
	;

floating_pt_type:	"float"				{ $$ = IDL_type_float_new(IDL_FLOAT_TYPE_FLOAT); }
|			"double"			{ $$ = IDL_type_float_new(IDL_FLOAT_TYPE_DOUBLE); }
|			"long" "double"			{ $$ = IDL_type_float_new(IDL_FLOAT_TYPE_LONGDOUBLE); }
	;

integer_type:		signed_int			{ $$ = IDL_type_integer_new(IDL_TRUE, $1); }
|			unsigned_int			{ $$ = IDL_type_integer_new(IDL_FALSE, $1); }
	;

signed_int:		signed_short_int		{ $$ = IDL_INTEGER_TYPE_SHORT; }
|			signed_long_int			{ $$ = IDL_INTEGER_TYPE_LONG; }
|			signed_longlong_int		{ $$ = IDL_INTEGER_TYPE_LONGLONG; }
	;

signed_short_int:	"short"
	;

signed_long_int:	"long"
	;

signed_longlong_int:	"long" "long"
	;

unsigned_int:		unsigned_short_int		{ $$ = IDL_INTEGER_TYPE_SHORT; }
|			unsigned_long_int		{ $$ = IDL_INTEGER_TYPE_LONG; }
|			unsigned_longlong_int		{ $$ = IDL_INTEGER_TYPE_LONGLONG; }
	;

unsigned_short_int:	"unsigned" "short"
	;

unsigned_long_int:	"unsigned" "long"
	;

unsigned_longlong_int:	"unsigned" "long" "long"
	;

char_type:		"char"			{ $$ = IDL_type_char_new(); }
	;

wide_char_type:		"wchar"			{ $$ = IDL_type_wide_char_new(); }
	;

boolean_type:		"boolean"		{ $$ = IDL_type_boolean_new(); }
	;

octet_type:		"octet"			{ $$ = IDL_type_octet_new(); }
	;

any_type:		"any"			{ $$ = IDL_type_any_new(); }
	;

object_type:		"Object"		{ $$ = IDL_type_object_new(); }
	;

declarator_list:	declarator			{ $$ = list_start($1); }
|			declarator_list ',' declarator	{ $$ = list_chain($1, $3); }
	;

declarator:		simple_declarator
/* |			complex_declarator */
	;

simple_declarator:	ident
	;


ident:			TOK_IDENT			{
	int added;

	IDL_tree p = IDL_ident_get(&__idl_symtab, $1, IDL_TRUE, &added);

	if (added == IDL_FALSE)
		yyerror("duplicate identifier");

	$$ = p;
}
	;

string_lit:		dqstring_cat			{
	$$ = IDL_string_new($1);
}
	;

dqstring_cat:		dqstring
|			dqstring_cat dqstring		{
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

sqstring:		TOK_SQSTRING			{
	do_escapes($1);
	$$ = $1;
}
	;

%%

void yyerrorl(const char *s, int ofs)
{
	int line = __idl_cur_line - 1 + ofs;

	++__idl_nerrors;
	
	if (__idl_cb)
		(*__idl_cb)(IDL_ERROR, __idl_nerrors, line,
			    __idl_cur_filename, s);
	else
		fprintf(stderr, "%s:%d: error [%d], %s\n", 
			__idl_cur_filename, line, __idl_nerrors, s);
}

void yywarningl(const char *s, int ofs)
{
	int line = __idl_cur_line - 1 + ofs;
	
	++__idl_nwarnings;
	
	if (__idl_cb)
		(*__idl_cb)(IDL_WARNING, __idl_nwarnings, line,
			    __idl_cur_filename, s);
	else
		fprintf(stderr, "%s:%d: warning [%d], %s\n",
			__idl_cur_filename, line, __idl_nwarnings, s);
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
	char *fmt = "unknown pragma: %s";
	char *msg = (char *)malloc(strlen(fmt) + strlen(s) - 2 + 1);
	sprintf(msg, fmt, s);
	yywarning("unknown pragma");
	free(msg);
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

void __idl_print_tree(IDL_tree p)
{
	if (!p)
		return;

	switch (IDL_NODE_TYPE(p)) {
	case IDLN_LIST:
		printf("IDL list\n");
		while (p) {
			__idl_print_tree(IDL_LIST(p).data);
			p = IDL_LIST(p).next;
		}
		break;

	case IDLN_STRING:
		printf("IDL string: %s\n", IDL_STRING(p).str);
		break;
		
	case IDLN_IDENT:
		printf("IDL ident: %s\n", IDL_IDENT(p).str);
		break;
		
	case IDLN_MEMBER:
		printf("IDL member declaration\n");
		__idl_print_tree(IDL_MEMBER(p).type_spec);
		__idl_print_tree(IDL_MEMBER(p).dcls);
		break;
		
	case IDLN_TYPE_DCL:
		printf("IDL type declaration\n");
		__idl_print_tree(IDL_TYPE_DCL(p).type_spec);
		__idl_print_tree(IDL_TYPE_DCL(p).dcls);
		break;
		
	case IDLN_TYPE_FLOAT:
		printf("IDL float type: %d\n", IDL_TYPE_FLOAT(p).f_type);
		break;

	case IDLN_TYPE_INTEGER:
		printf("IDL integer type: %d %d\n",
		       IDL_TYPE_INTEGER(p).f_signed,
		       IDL_TYPE_INTEGER(p).f_type);
		break;

	case IDLN_TYPE_CHAR:
		printf("IDL char type\n");
		break;
		
	case IDLN_TYPE_WIDE_CHAR:
		printf("IDL wide char type\n");
		break;
		
	case IDLN_TYPE_BOOLEAN:
		printf("IDL boolean type\n");
		break;
		
	case IDLN_TYPE_OCTET:
		printf("IDL octet type\n");
		break;
		
	case IDLN_TYPE_ANY:
		printf("IDL any type\n");
		break;
		
	case IDLN_TYPE_OBJECT:
		printf("IDL object type\n");
		break;

	case IDLN_TYPE_STRUCT:
		printf("IDL struct type\n");
		__idl_print_tree(IDL_TYPE_STRUCT(p).ident);
		__idl_print_tree(IDL_TYPE_STRUCT(p).member_list);
		break;
		
	case IDLN_TYPE_UNION:
		printf("IDL union type\n");
		__idl_print_tree(IDL_TYPE_UNION(p).ident);
		__idl_print_tree(IDL_TYPE_UNION(p).switch_type_spec);
		__idl_print_tree(IDL_TYPE_UNION(p).switch_body);
		break;
		
	default:
		fprintf(stderr, "warning: print unknown node: %d\n", IDL_NODE_TYPE(p));
		break;
	}
}

static void __idl_tree_free(IDL_tree p, int idents)
{
	IDL_tree q;

	if (!p)
		return;

	switch (IDL_NODE_TYPE(p)) {
	case IDLN_LIST:
		while (p) {
			__idl_tree_free(IDL_LIST(p).data, idents);
			q = IDL_LIST(p).next;
			free(p);
			p = q;
		}
		break;

	case IDLN_STRING:
		free(IDL_STRING(p).str);
		free(p);
		break;

	case IDLN_IDENT:
		if (idents == IDL_TRUE) {
			free(IDL_IDENT(p).str);
			free(p);
		}
		break;

	case IDLN_MEMBER:
		__idl_tree_free(IDL_MEMBER(p).type_spec, idents);
		__idl_tree_free(IDL_MEMBER(p).dcls, idents);
		break;

	case IDLN_TYPE_ENUM:
		__idl_tree_free(IDL_TYPE_ENUM(p).ident, idents);
		__idl_tree_free(IDL_TYPE_ENUM(p).enumerator_list, idents);
		break;

	case IDLN_TYPE_STRUCT:
		__idl_tree_free(IDL_TYPE_STRUCT(p).ident, idents);
		__idl_tree_free(IDL_TYPE_STRUCT(p).member_list, idents);
		break;

	case IDLN_TYPE_UNION:
		__idl_tree_free(IDL_TYPE_UNION(p).ident, idents);
		__idl_tree_free(IDL_TYPE_UNION(p).switch_type_spec, idents);
		__idl_tree_free(IDL_TYPE_UNION(p).switch_body, idents);
		break;
				
	case IDLN_TYPE_DCL:
		__idl_tree_free(IDL_TYPE_DCL(p).type_spec, idents);
		__idl_tree_free(IDL_TYPE_DCL(p).dcls, idents);
		free(p);
		break;

	case IDLN_TYPE_FLOAT:		
	case IDLN_TYPE_INTEGER:
	case IDLN_TYPE_CHAR:
	case IDLN_TYPE_WIDE_CHAR:
	case IDLN_TYPE_BOOLEAN:
	case IDLN_TYPE_OCTET:
	case IDLN_TYPE_ANY:
		free(p);
		break;
		
	default:
		fprintf(stderr, "warning: free unknown node: %d\n", IDL_NODE_TYPE(p));
		break;
	}
}

void IDL_root_free(IDL_tree root)
{
	__idl_tree_free(root, IDL_FALSE);
}

void IDL_symtab_free(IDL_tree symtab)
{
	__idl_tree_free(symtab, IDL_TRUE);
}

static IDL_tree list_start(IDL_tree a)
{
	IDL_tree p = IDL_list_new(a);
	IDL_LIST(p)._tail = p;

	return p;
}

static IDL_tree list_chain(IDL_tree a, IDL_tree b)
{
	IDL_tree p = IDL_list_new(b);
	IDL_LIST(IDL_LIST(a)._tail).next = p;
	IDL_LIST(a)._tail = p;

	return a;
}

static IDL_tree IDL_node_new(IDL_tree_type type)
{
	IDL_tree p;

	p = (IDL_tree)malloc(sizeof(IDL_tree_node));
	assert(p != NULL);
	memset(p, 0, sizeof(IDL_tree_node));
	IDL_NODE_TYPE(p) = type;

	return p;
}

IDL_tree IDL_list_new(IDL_tree data)
{
	IDL_tree p = IDL_node_new(IDLN_LIST);
	
	IDL_LIST(p).data = data;
	
	return p;
}

IDL_tree IDL_string_new(char *s)
{
	IDL_tree p = IDL_node_new(IDLN_STRING);

	IDL_STRING(p).str = s;

	return p;
}

IDL_tree IDL_ident_get(IDL_tree *symtab, char *s_ident, int add, int *added)
{
	IDL_tree p, ident, found = NULL;

	if (!s_ident)
		return NULL;

	if (added)
		*added = IDL_FALSE;

	if (symtab) {
		for (p = *symtab;
		     p && IDL_NODE_TYPE(p) == IDLN_LIST;
		     p = IDL_LIST(p).next) {
			
			if (IDL_NODE_TYPE(IDL_LIST(p).data) == IDLN_IDENT
			    && strcmp(IDL_IDENT(IDL_LIST(p).data).str, s_ident) == 0) {
				found = IDL_LIST(p).data;
				break;
			}
		}
		
		if (found) {
			free(s_ident);
			return found;
		}
	}
	
	if (add == IDL_FALSE)
		return NULL;

	if (added)
		*added = IDL_TRUE;
	
	ident = IDL_node_new(IDLN_IDENT);
	IDL_IDENT(ident).str = s_ident;

	if (!*symtab)
		*symtab = list_start(ident);
	else
		list_chain(*symtab, ident);

	return ident;
}

IDL_tree IDL_member_new(IDL_tree type_spec, IDL_tree dcls)
{
	IDL_tree p = IDL_node_new(IDLN_MEMBER);

	IDL_MEMBER(p).type_spec = type_spec;
	IDL_MEMBER(p).dcls = dcls;
	
	return p;
}

IDL_tree IDL_type_dcl_new(IDL_tree type_spec, IDL_tree dcls)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_DCL);

	IDL_TYPE_DCL(p).type_spec = type_spec;
	IDL_TYPE_DCL(p).dcls = dcls;
	
	return p;
}

IDL_tree IDL_type_float_new(unsigned f_type)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_FLOAT);
	
	IDL_TYPE_FLOAT(p).f_type = f_type;

	return p;
}

IDL_tree IDL_type_integer_new(unsigned f_signed, unsigned f_type)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_INTEGER);
	
	IDL_TYPE_INTEGER(p).f_signed = f_signed;
	IDL_TYPE_INTEGER(p).f_type = f_type;

	return p;
}

IDL_tree IDL_type_char_new(void)
{
	return IDL_node_new(IDLN_TYPE_CHAR);
}

IDL_tree IDL_type_wide_char_new(void)
{
	return IDL_node_new(IDLN_TYPE_WIDE_CHAR);
}

IDL_tree IDL_type_boolean_new(void)
{
	return IDL_node_new(IDLN_TYPE_BOOLEAN);
}

IDL_tree IDL_type_octet_new(void)
{
	return IDL_node_new(IDLN_TYPE_OCTET);
}

IDL_tree IDL_type_any_new(void)
{
	return IDL_node_new(IDLN_TYPE_ANY);
}

IDL_tree IDL_type_object_new(void)
{
	return IDL_node_new(IDLN_TYPE_OBJECT);
}

IDL_tree IDL_type_struct_new(IDL_tree ident, IDL_tree member_list)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_STRUCT);
	
	IDL_TYPE_STRUCT(p).ident = ident;
	IDL_TYPE_STRUCT(p).member_list = member_list;

	return p;
}

IDL_tree IDL_type_union_new(IDL_tree ident, IDL_tree switch_type_spec, IDL_tree switch_body)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_UNION);
	
	IDL_TYPE_UNION(p).ident = ident;
	IDL_TYPE_UNION(p).switch_type_spec = switch_type_spec;
	IDL_TYPE_UNION(p).switch_body = switch_body;

	return p;
}

IDL_tree IDL_type_enum_new(IDL_tree ident, IDL_tree enumerator_list)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_ENUM);
	
	IDL_TYPE_ENUM(p).ident = ident;
	IDL_TYPE_ENUM(p).enumerator_list = enumerator_list;

	return p;
}
