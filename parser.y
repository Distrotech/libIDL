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
	long integer_v;
	float float_v;
	double double_v;
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

%token<str>			TOK_IDENT TOK_SQSTRING TOK_DQSTRING
%token<integer>			TOK_INTEGER
%token<floatp>			TOK_FLOATP
%token<fixedp>			TOK_FIXEDP

%type<str>			string_lit dqstring ident
%type<tree>			specification
%type<tree>			definition_list definition
%type<tree>			type_dcl const_dcl except_dcl interface module

%%

start:			idl_init
			specification
			idl_finish			{
	if (okay)
		__idl_root = $2;
}
	;

idl_init:						{
	__idl_root = NULL;
	okay = 1;
}
	;

idl_finish:						{
}
	;

specification:		definition_list			{
	$$ = $1;
}
	;

definition_list:	definition
|			definition_list definition
	;

definition:		type_dcl ';'
|			const_dcl ';'
|			except_dcl ';'
|			interface ';'
|			module ';'
	;

interface:		interface_dcl
|			forward_dcl
	;

module:			"module" ident '{'
				definition_list
			'}'				{
	$$ = NULL;
}
	;

interface_dcl:		interface_header '{'
				z_interface_body
			'}'
	;

forward_dcl:		"interface" ident
	;

interface_header:	"interface" ident z_inheritence_spec
	;

z_inheritence_spec:	/* empty */
|			inheritence_spec
	;

inheritence_spec:	':' scoped_name_list
	;

scoped_name_list:	scoped_name
|			scoped_name_list ',' scoped_name
	;

scoped_name:		ident
|			"::" ident
|			scoped_name "::" ident
	;

z_interface_body:	/* empty */
|			export_list
	;

export_list:		export
|			export_list
	;

export:			type_dcl ';'
|			const_dcl ';'
|			except_dcl ';'
|			attr_dcl ';'
|			op_dcl ';'
	;

const_dcl:		"const" const_type ident '=' const_exp
	;

const_type:		integer_type
|			char_type
|			boolean_type
|			floating_pt_type
|			string_type
|			scoped_name
	;

const_expr:		or_expr
	;

or_expr:		xor_expr
|			or_expr '|' xor_expr
	;

xor_expr:		and_expr
|			xor_expr '^' and_expr
	;

and_expr:		shift_expr
|			and_expr '&' shift_expr
	;

shift_expr:		add_expr
|			shift_expr ">>" add_expr
|			shift_expr "<<" add_expr
	;

add_expr:		mult_expr
|			add_expr '+' mult_expr
|			add_expr '-' mult_expr
	;

mult_expr:		unary_expr
|			mult_expr '*' unary_expr
|			mult_expr '/' unary_expr
|			mult_expr '%' unary_expr
	;

unary_expr:		unary_op primary_expr
|			primary_expr
	;

unary_op:		'-'
|			'+'
|			'~'
	;

primary_expr:		scoped_name
|			literal
|			'(' const_expr ')'
	;

literal:		integer_lit
|			string_lit
|			character_lit
|			floating_pt_lit
|			boolean_lit
	;

boolean_lit:		"TRUE"
|			"FALSE"
	;

positive_int_const:	const_expr
	;

type_dcl:		"typedef" type_declarator
|			struct_type
|			union_type
|			enum_type
|			"native" simple_declarator
	;

type_declarator:	type_spec declarator_list
	;

type_spec:		simple_type_spec
|			constr_type_spec
	;

simple_type_spec:	base_type_spec
|			template_type_spec
|			scoped_name
	;

base_type_spec:		floating_pt_type
|			integer_type
|			char_type
|			wide_char_type
|			boolean_type
|			octet_type
|			any_type
	;

template_type_spec:	sequence_type
|			string_type
|			wide_string_type
|			fixed_pt_type
	;

constr_type_spec:	struct_type
|			union_type
|			enum_type
	;

declarator_list:	declarator
|			declarator_list ',' declarator
	;

declarator:		simple_declarator
|			complex_declarator
	;

simple_declarator:	ident
	;

complex_declarator:	array_declarator
	;

floating_pt_type:	"float"
|			"double"
|			"long" "double"
	;

integer_type:		signed_int
|			unsigned_int
	;

signed_int:		signed_long_int
|			signed_short_int
|			signed_longlong_int
	;

signed_long_int:	"long"
	;

signed_short_int:	"short"
	;

signed_longlong_int:	"long" "long"
	;

unsigned_long_int:	"unsigned" "long"
	;

unsigned_short_int:	"unsigned" "short"
	;

unsigned_longlong_int:	"unsigned" "long" "long"
	;

char_type:		"char"
	;

wide_char_type:		"wchar"
	;

boolean_type:		"boolean"
	;

octet_type:		"octet"
	;

any_type:		"any"
	;

constr_type_spec:	struct_type
|			union_type
|			enum_type
	;

struct_type:		"struct" ident '{'
				member_list
			'}'
	;

member_list:		member
|			member_list member
	;

member:			type_spec declarator_list ';'
	;

union_type:		"union" ident "siwtch" '(' switch_type_spec ')' '{'
				switch_body
			'}'
	;

switch_body:		case_list
	;

case_list:		case
|			case_list case
	;

case:			case_label_list element_spec ';'
	;

case_label_list:	case_label
|			case_label_list case_label
	;

case_label:		"case" const_expr ':'
|			"default" ':'
	;

element_spec:		type_spec declarator
	;

enum_type:		"enum" ident '{'
				enumerator_list
			'}'
	;

enumerator_list:	enumerator
|			enumerator_list enumerator
	;

enumerator:		ident
	;

template_type_spec:	sequence_type
|			string_type
|			wide_string_type
|			fixed_pt_type
	;

sequence_type:		"sequence" '<' simple_type_spec ',' positive_int_const '>'
|			"sequence" '<' simple_type_spec '>'
	;

string_type:		"string" '<' positive_int_const '>'
|			"string"
	;

wide_string_type:	"wstring" '<' positive_int_const '>'
|			"wstring"
	;

array_declarator:	ident fixed_array_size_list
	;

fixed_array_size_list:	fixed_array_size
|			fixed_array_size_list fixed_array_size
	;

fixed_array_size:	'[' positive_int_const ']'
	;

type_dcl:		"native" simple_declarator
	;

simple_declarator:	ident
	;

except_dcl:		"exception" ident '{'
				z_member_list
			'}'
	;

z_member_list:		/* empty */
|			member_list
	;

op_dcl:			z_op_attribute op_type_spec ident
			paramter_dcls z_raises_expr z_context_expr
	;

op_type_spec:		param_type_spec
|			"void"
	;

z_op_attribute:		/* empty */
|			op_attribute
	;

z_raises_expr:		/* empty */
|			raises_expr
	;

z_context_expr:		/* empty */
|			context_expr
	;

op_attribute:		"oneway"
	;

parameter_dcls:		'(' param_dcl_list ')'
|			'(' ')'
	;

param_dcl:		param_attribute param_type_spec simple_declarator
	;

param_attribute:	"in"
|			"out"
|			"inout"
	;

param_type_spec:	base_type_spec
|			string_type
|			scoped_name
	;

raises_expr:		"raises" '(' scoped_name_list ')'
	;

context_expr:		"context" '(' string_lit_list ')'
	;

string_lit_list:	string_lit
|			string_lit_list string_lit
	;

attr_dcl:		z_readonly "attribute" param_type_spec
			simple_declarator_list
	;

z_readonly:		/* empty */
|			"readonly"
	;

ident:			TOK_IDENT
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
	char *fmt = "unknown pragma: %s";
	char *msg = (char *)malloc(strlen(fmt) + strlen(s) - 2 + 1);
	sprintf(msg, fmt, s);
	yywarning("unknown pragma");
	free(msg);
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
