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
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "idl.h"
#include "rename.h"
#include "util.h"

static void			do_escapes(char *s);
static IDL_tree *		IDL_tree_node_new(void);

static IDL_tree			__idl_root, __idl_symtab;
static IDL_callback		__idl_cb = NULL;
static int			okay;
static int			__idl_nerrors, __idl_nwarnings;

char *				__idl_cur_filename = NULL;
int				__idl_cur_line;

void				__idl_print_tree(IDL_tree p);

static IDL_tree			list_start(IDL_tree a);
static IDL_tree			list_chain(IDL_tree a, IDL_tree b);
static IDL_tree			zlist_chain(IDL_tree a, IDL_tree b);
%}

%union {
	IDL_tree tree;
	char *str;
	long integer;
	double floatp;
	double fixedp;
	int boolean;
	enum IDL_unaryop unaryop;
	enum IDL_param_attr paramattr;
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
%type <tree>			type_dcl const_dcl except_dcl attr_dcl op_dcl
%type <tree>			type_spec type_declarator declarator_list declarator
%type <tree>			simple_type_spec constr_type_spec base_type_spec
%type <tree>			template_type_spec sequence_type
%type <tree>			simple_declarator complex_declarator array_declarator
%type <tree>			simple_declarator_list
%type <tree>			struct_type union_type
%type <tree>			member member_list member_zlist enumerator_list
%type <tree>			switch_type_spec switch_body
%type <tree>			interface interface_dcl forward_dcl
%type <tree>			interface_body z_scoped_name scoped_name_list
%type <tree>			export_list export
%type <tree>			module const_type
%type <tree>			ident floating_pt_type integer_type
%type <tree>			char_type wide_char_type boolean_type octet_type
%type <tree>			string_type wide_string_type fixed_pt_type fixed_pt_const_type
%type <tree>			any_type object_type enum_type scoped_name
%type <tree>			case_list case_label fixed_array_size_list
%type <tree>			fixed_array_size positive_int_const

%type <tree>			param_type_spec op_type_spec parameter_dcls
%type <tree>			is_raises_expr is_context_expr param_dcl_list
%type <tree>			param_dcl raises_expr context_expr

%type <tree>			const_exp or_expr xor_expr and_expr shift_expr
%type <tree>			add_expr mult_expr unary_expr primary_expr literal

%type <tree>			integer_lit
%type <tree>			string_lit char_lit /* wide_string_lit wide_char_lit */ 
%type <tree>			fixed_pt_lit floating_pt_lit
%type <tree>			boolean_lit
%type <tree>			string_lit_list

%type <unaryop>			unary_op
%type <paramattr>		param_attribute

%type <integer>			signed_int unsigned_int is_readonly is_oneway

%start start

%%

idl_init:						{
	okay = 1;
	__idl_nerrors = __idl_nwarnings = 0;
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

specification:		/* empty */			{ yywarning("file is empty"); }
|			definition_list
	;

definition_list:	definition			{ $$ = list_start($1); }
|			definition_list definition	{ $$ = list_chain($1, $2); }
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
			'}'				{ $$ = IDL_module_new($2, $4); }
	;

interface_dcl:		"interface" ident z_scoped_name '{'
				interface_body
			'}'				{ $$ = IDL_interface_new($2, $3, $5); }
	;

forward_dcl:		"interface" ident		{ $$ = IDL_forward_dcl_new($2); }
	;

z_scoped_name:		/* empty */			{ $$ = NULL; }
|			':' scoped_name_list		{ $$ = $2; }
	;

scoped_name_list:	scoped_name			{ $$ = list_start($1); }
|			scoped_name_list
			',' scoped_name			{ $$ = list_chain($1, $3); }
	;

interface_body:		export_list
	;

export_list:		/* empty */			{ $$ = NULL; }
|			export_list export		{ $$ = zlist_chain($1, $2); }
	;

export:			type_dcl ';'
|			const_dcl ';'
|			except_dcl ';'
|			attr_dcl ';'
|			op_dcl ';'
	;

type_dcl:		"typedef" type_declarator	{ $$ = $2; }
|			struct_type
|			union_type
|			enum_type
|			"native" simple_declarator	{ $$ = $2; }
	;

type_declarator:	type_spec declarator_list	{ $$ = IDL_type_dcl_new($1, $2); }
	;

type_spec:		simple_type_spec
|			constr_type_spec
	;

simple_type_spec:	base_type_spec
|			template_type_spec
|			scoped_name
	;

constr_type_spec:	struct_type
|			union_type
|			enum_type
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

switch_body:		case_list
	;

case_list:		case_label			{ $$ = list_start($1); }
|			case_list case_label		{ $$ = list_chain($1, $2); }
	;

case_label:		"case" const_exp ':'		{ $$ = IDL_case_label_new($2); }
|			"default" ':'			{ $$ = IDL_case_label_new(NULL); }
	;

const_dcl:		"const" const_type ident
			'=' const_exp			{ $$ = IDL_const_dcl_new($2, $3, $5); }
	;

except_dcl:		"exception" ident '{'
				member_zlist
			'}'				{ $$ = IDL_except_dcl_new($2, $4); }
	;

member_zlist:		/* empty */			{ $$ = NULL; }
|			member_zlist member		{ $$ = zlist_chain($1, $2); }
	;

is_readonly:		/* empty */			{ $$ = IDL_FALSE; }
|			"readonly"			{ $$ = IDL_TRUE; }
	;

attr_dcl:		is_readonly "attribute"
			param_type_spec
			simple_declarator_list		{ $$ = IDL_attr_dcl_new($1, $3, $4); }
	;

param_type_spec:	base_type_spec
|			string_type
|			wide_string_type
|			fixed_pt_type
|			scoped_name
	;

is_oneway:		/* empty */			{ $$ = IDL_FALSE; }
|			"oneway"			{ $$ = IDL_TRUE; }
	;

op_dcl:			is_oneway op_type_spec ident
			parameter_dcls
			is_raises_expr
			is_context_expr			{ $$ = $2; }
	;

op_type_spec:		param_type_spec
|			"void"				{ $$ = NULL; }
	;

parameter_dcls:		'(' param_dcl_list ')'		{ $$ = $2; }
|			'(' ')'				{ $$ = NULL; }
	;

param_dcl_list:		param_dcl			{ $$ = list_start($1); }
|			param_dcl_list ',' param_dcl	{ $$ = list_chain($1, $3); }
	;

param_dcl:		param_attribute
			param_type_spec
			simple_declarator		{ $$ = IDL_param_dcl_new($1, $2, $3); }
	;

param_attribute:	"in"				{ $$ = IDL_PARAM_IN; }
|			"out"				{ $$ = IDL_PARAM_OUT; }
|			"inout"				{ $$ = IDL_PARAM_INOUT; }
	;

is_raises_expr:		/* empty */			{ $$ = NULL; }
|			raises_expr
	;

is_context_expr:	/* empty */			{ $$ = NULL; }
|			context_expr
	;

raises_expr:		"raises" '('
				scoped_name_list
			')'				{ $$ = $3; }
	;

context_expr:		"context" '('
				string_lit_list
			')'				{ $$ = $3; }
	;

const_type:		integer_type
|			char_type
|			wide_char_type
|			boolean_type
|			floating_pt_type
|			string_type
|			wide_string_type
|			fixed_pt_const_type
|			scoped_name
	;

const_exp:		or_expr
	;

or_expr:		xor_expr
|			or_expr '|' xor_expr		{ $$ = IDL_binop_new(IDL_BINOP_OR, $1, $3); }
	;

xor_expr:		and_expr
|			xor_expr '^' and_expr		{ $$ = IDL_binop_new(IDL_BINOP_XOR, $1, $3); }
	;

and_expr:		shift_expr
|			and_expr '&' shift_expr		{ $$ = IDL_binop_new(IDL_BINOP_AND, $1, $3); }
	;

shift_expr:		add_expr
|			shift_expr ">>" add_expr	{ $$ = IDL_binop_new(IDL_BINOP_SHR, $1, $3); }
|			shift_expr "<<" add_expr	{ $$ = IDL_binop_new(IDL_BINOP_SHL, $1, $3); }
	;

add_expr:		mult_expr
|			add_expr '+' mult_expr		{ $$ = IDL_binop_new(IDL_BINOP_ADD, $1, $3); }
|			add_expr '-' mult_expr		{ $$ = IDL_binop_new(IDL_BINOP_SUB, $1, $3); }
	;

mult_expr:		unary_expr
|			mult_expr '*' unary_expr	{ $$ = IDL_binop_new(IDL_BINOP_MULT, $1, $3); }
|			mult_expr '/' unary_expr	{ $$ = IDL_binop_new(IDL_BINOP_DIV, $1, $3); }
|			mult_expr '%' unary_expr	{ $$ = IDL_binop_new(IDL_BINOP_MOD, $1, $3); }
	;

unary_expr:		unary_op primary_expr		{ $$ = IDL_unaryop_new($1, $2); }
|			primary_expr
	;

unary_op:		'-'				{ $$ = IDL_UNARYOP_MINUS; }
|			'+'				{ $$ = IDL_UNARYOP_PLUS; }
|			'~'				{ $$ = IDL_UNARYOP_COMPLEMENT; }
	;

primary_expr:		scoped_name
|			literal
|			'(' const_exp ')'		{ $$ = $2; }
	;

literal:		integer_lit
|			string_lit
/*
|			wide_string_lit
*/
|			char_lit
/*
|			wide_char_lit
*/
|			fixed_pt_lit
|			floating_pt_lit
|			boolean_lit
	;

enum_type:		"enum" ident '{'
				enumerator_list
			'}'				{ $$ = IDL_type_enum_new($2, $4); }
	;

scoped_name:		ident				{ $$ = list_start($1); }
|			"::" ident			{ $$ = list_start($2); }
|			scoped_name "::" ident		{ $$ = list_chain($1, $3); }
	;

enumerator_list:	ident				{ $$ = list_start($1); }
|			enumerator_list ',' ident	{ $$ = list_chain($1, $3); }
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

template_type_spec:	sequence_type
|			string_type
|			wide_string_type
|			fixed_pt_type
	;

sequence_type:		"sequence" '<'
				simple_type_spec ',' positive_int_const
			'>'				{ $$ = IDL_type_sequence_new($3, $5); }
|			"sequence" '<'
				simple_type_spec
			'>'				{ $$ = IDL_type_sequence_new($3, NULL); }
	;

floating_pt_type:	"float"				{ $$ = IDL_type_float_new(IDL_FLOAT_TYPE_FLOAT); }
|			"double"			{ $$ = IDL_type_float_new(IDL_FLOAT_TYPE_DOUBLE); }
|			"long" "double"			{ $$ = IDL_type_float_new(IDL_FLOAT_TYPE_LONGDOUBLE); }
	;

fixed_pt_type:		"fixed" '<'
				positive_int_const ',' integer_lit
			'>'				{ $$ = IDL_type_fixed_new($3, $5); }
	;

fixed_pt_const_type:	"fixed"				{ $$ = IDL_type_fixed_new(NULL, NULL); }
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

string_type:		"string" '<'
				positive_int_const
				'>'		{ $$ = IDL_type_string_new($3); }
|			"string"		{ $$ = IDL_type_string_new(NULL); }
	;

wide_string_type:	"wstring" '<'
				positive_int_const
			'>'			{ $$ = IDL_type_wide_string_new($3); }
|			"wstring"		{ $$ = IDL_type_wide_string_new(NULL); }
	;

declarator_list:	declarator		{ $$ = list_start($1); }
|			declarator_list 
			',' declarator		{ $$ = list_chain($1, $3); }
	;

declarator:		simple_declarator
|			complex_declarator
	;

simple_declarator:	ident
	;

complex_declarator:	array_declarator
	;

simple_declarator_list:	simple_declarator		{ $$ = list_start($1); }
|			simple_declarator_list
			simple_declarator		{ $$ = list_chain($1, $2); }
	;


array_declarator:	ident fixed_array_size_list	{ $$ = IDL_type_array_new($1, $2); }
	;

fixed_array_size_list:	fixed_array_size		{ $$ = list_start($1); }
|			fixed_array_size_list
			fixed_array_size		{ $$ = list_chain($1, $2); }
	;

fixed_array_size:	'[' positive_int_const ']'	{ $$ = $2; }
	;

ident:			TOK_IDENT			{
	int added;

	IDL_tree p = IDL_ident_get(&__idl_symtab, $1, IDL_TRUE, &added);

	$$ = p;
}
	;

string_lit_list:	string_lit			{ $$ = list_start($1); }
|			string_lit_list ',' string_lit	{ $$ = list_chain($1, $3); }
	;

positive_int_const:	TOK_INTEGER			{
	if ($1 < 0) {
		yyerror("cannot use negative value, using absolute value");
		$$ = IDL_integer_new(-$1);
	}
	else
		$$ = IDL_integer_new($1);
}
	;


integer_lit:		TOK_INTEGER			{ $$ = IDL_integer_new($1); }
	;

string_lit:		dqstring_cat			{ $$ = IDL_string_new($1); }
	;

char_lit:		sqstring			{ $$ = IDL_char_new($1); }
	;

fixed_pt_lit:		TOK_FIXEDP			{ $$ = IDL_fixed_new($1); }
	;

floating_pt_lit:	TOK_FLOATP			{ $$ = IDL_float_new($1); }
	;

boolean_lit:		"TRUE"				{ $$ = IDL_boolean_new(IDL_TRUE); }
|			"FALSE"				{ $$ = IDL_boolean_new(IDL_FALSE); }
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
		(*__idl_cb)(IDL_ERROR, __idl_nerrors, line, __idl_cur_filename, s);
	else
		fprintf(stderr, "%s:%d: error [%d], %s\n", 
			__idl_cur_filename, line, __idl_nerrors, s);
}

void yywarningl(const char *s, int ofs)
{
	int line = __idl_cur_line - 1 + ofs;
	
	++__idl_nwarnings;
	
	if (__idl_cb)
		(*__idl_cb)(IDL_WARNING, __idl_nwarnings, line, __idl_cur_filename, s);
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
	yywarning(msg);
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
	if (p == NULL)
		return;

	switch (IDL_NODE_TYPE(p)) {
	case IDLN_LIST:
		printf("IDL list\n");
		while (p) {
			__idl_print_tree(IDL_LIST(p).data);
			p = IDL_LIST(p).next;
		}
		break;

	case IDLN_INTEGER:
		printf("IDL integer: %d\n", IDL_INTEGER(p).value);
		break;
		
	case IDLN_STRING:
		printf("IDL string: %s\n", IDL_STRING(p).value);
		break;
		
	case IDLN_CHAR:
		printf("IDL char: %s\n", IDL_CHAR(p).value);
		break;
		
	case IDLN_FIXED:
		printf("IDL fixed: %g\n", IDL_FIXED(p).value);
		break;
		
	case IDLN_FLOAT:
		printf("IDL float: %g\n", IDL_FLOAT(p).value);
		break;
		
	case IDLN_BOOLEAN:
		printf("IDL boolean: %s\n",
		       IDL_BOOLEAN(p).value ? "TRUE" : "FALSE");
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

	case IDLN_CONST_DCL:
		printf("IDL const declaration\n");
		__idl_print_tree(IDL_CONST_DCL(p).const_type);
		__idl_print_tree(IDL_CONST_DCL(p).ident);
		__idl_print_tree(IDL_CONST_DCL(p).const_exp);
		break;
		
	case IDLN_EXCEPT_DCL:
		printf("IDL exception declaration\n");
		__idl_print_tree(IDL_EXCEPT_DCL(p).ident);
		__idl_print_tree(IDL_EXCEPT_DCL(p).members);
		break;
		
	case IDLN_ATTR_DCL:
		printf("IDL attr declaration\n");
		__idl_print_tree(IDL_ATTR_DCL(p).param_type_spec);
		__idl_print_tree(IDL_ATTR_DCL(p).simple_declarations);
		break;
		
	case IDLN_OP_DCL:
		printf("IDL op declaration\n");
		__idl_print_tree(IDL_OP_DCL(p).op_type_spec);
		__idl_print_tree(IDL_OP_DCL(p).ident);
		__idl_print_tree(IDL_OP_DCL(p).parameter_dcls);
		__idl_print_tree(IDL_OP_DCL(p).raises_expr);
		__idl_print_tree(IDL_OP_DCL(p).context_expr);
		break;

	case IDLN_PARAM_DCL:
		printf("IDL param declaration: %d\n", IDL_PARAM_DCL(p).attr);
		__idl_print_tree(IDL_PARAM_DCL(p).param_type_spec);
		__idl_print_tree(IDL_PARAM_DCL(p).simple_declarator);
		break;

	case IDLN_FORWARD_DCL:
		printf("IDL forward declaration\n");
		__idl_print_tree(IDL_FORWARD_DCL(p).ident);
		break;
		
	case IDLN_TYPE_FLOAT:
		printf("IDL float type: %d\n", IDL_TYPE_FLOAT(p).f_type);
		break;

	case IDLN_TYPE_FIXED:
		printf("IDL fixed type\n");
		__idl_print_tree(IDL_TYPE_FIXED(p).positive_int_const);
		__idl_print_tree(IDL_TYPE_FIXED(p).integer_lit);
		break;

	case IDLN_TYPE_INTEGER:
		printf("IDL integer type: %d %d\n",
		       IDL_TYPE_INTEGER(p).f_signed,
		       IDL_TYPE_INTEGER(p).f_type);
		break;

	case IDLN_TYPE_STRING:
		printf("IDL string type\n");
		__idl_print_tree(IDL_TYPE_STRING(p).positive_int_const);
		break;

	case IDLN_TYPE_WIDE_STRING:
		printf("IDL wide string type\n");
		__idl_print_tree(IDL_TYPE_WIDE_STRING(p).positive_int_const);
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

	case IDLN_TYPE_ENUM:
		printf("IDL enum type\n");
		break;

	case IDLN_TYPE_SEQUENCE:
		printf("IDL sequence type\n");
		__idl_print_tree(IDL_TYPE_SEQUENCE(p).simple_type_spec);
		__idl_print_tree(IDL_TYPE_SEQUENCE(p).positive_int_const);
		break;

	case IDLN_TYPE_ARRAY:
		printf("IDL array type\n");
		__idl_print_tree(IDL_TYPE_ARRAY(p).ident);
		__idl_print_tree(IDL_TYPE_ARRAY(p).size_list);
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

	case IDLN_CASE_LABEL:
		__idl_print_tree(IDL_CASE_LABEL(p).const_exp);
		break;

	case IDLN_INTERFACE:
		__idl_print_tree(IDL_INTERFACE(p).ident);
		__idl_print_tree(IDL_INTERFACE(p).inheritence_spec);
		__idl_print_tree(IDL_INTERFACE(p).body);
		break;

	case IDLN_MODULE:
		__idl_print_tree(IDL_MODULE(p).ident);
		__idl_print_tree(IDL_MODULE(p).definition_list);
		break;		

	case IDLN_BINOP:
		printf("IDL binop: op %d\n", IDL_BINOP(p).op);
		__idl_print_tree(IDL_BINOP(p).left);
		__idl_print_tree(IDL_BINOP(p).right);
		break;

	case IDLN_UNARYOP:
		printf("IDL unary: op %d\n", IDL_UNARYOP(p).op);
		__idl_print_tree(IDL_UNARYOP(p).operand);
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

	case IDLN_INTEGER:
	case IDLN_FIXED:
	case IDLN_FLOAT:
	case IDLN_BOOLEAN:
		free(p);
		break;

	case IDLN_STRING:
		free(IDL_STRING(p).value);
		free(p);
		break;

	case IDLN_CHAR:
		free(IDL_CHAR(p).value);
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
		free(p);
		break;

	case IDLN_TYPE_ENUM:
		__idl_tree_free(IDL_TYPE_ENUM(p).ident, idents);
		__idl_tree_free(IDL_TYPE_ENUM(p).enumerator_list, idents);
		free(p);
		break;

	case IDLN_TYPE_SEQUENCE:
		__idl_tree_free(IDL_TYPE_SEQUENCE(p).simple_type_spec, idents);
		__idl_tree_free(IDL_TYPE_SEQUENCE(p).positive_int_const, idents);
		free(p);
		break;

	case IDLN_TYPE_ARRAY:
		__idl_tree_free(IDL_TYPE_ARRAY(p).ident, idents);
		__idl_tree_free(IDL_TYPE_ARRAY(p).size_list, idents);
		free(p);
		break;

	case IDLN_TYPE_STRUCT:
		__idl_tree_free(IDL_TYPE_STRUCT(p).ident, idents);
		__idl_tree_free(IDL_TYPE_STRUCT(p).member_list, idents);
		free(p);
		break;

	case IDLN_TYPE_UNION:
		__idl_tree_free(IDL_TYPE_UNION(p).ident, idents);
		__idl_tree_free(IDL_TYPE_UNION(p).switch_type_spec, idents);
		__idl_tree_free(IDL_TYPE_UNION(p).switch_body, idents);
		free(p);
		break;
				
	case IDLN_TYPE_DCL:
		__idl_tree_free(IDL_TYPE_DCL(p).type_spec, idents);
		__idl_tree_free(IDL_TYPE_DCL(p).dcls, idents);
		free(p);
		break;

	case IDLN_CONST_DCL:
		__idl_tree_free(IDL_CONST_DCL(p).const_type, idents);
		__idl_tree_free(IDL_CONST_DCL(p).ident, idents);
		__idl_tree_free(IDL_CONST_DCL(p).const_exp, idents);
		free(p);
		break;

	case IDLN_EXCEPT_DCL:
		__idl_tree_free(IDL_EXCEPT_DCL(p).ident, idents);
		__idl_tree_free(IDL_EXCEPT_DCL(p).members, idents);
		free(p);
		break;
		
	case IDLN_ATTR_DCL:
		__idl_tree_free(IDL_ATTR_DCL(p).param_type_spec, idents);
		__idl_tree_free(IDL_ATTR_DCL(p).simple_declarations, idents);
		free(p);
		break;
		
	case IDLN_OP_DCL:
		__idl_tree_free(IDL_OP_DCL(p).op_type_spec, idents);
		__idl_tree_free(IDL_OP_DCL(p).ident, idents);
		__idl_tree_free(IDL_OP_DCL(p).parameter_dcls, idents);
		__idl_tree_free(IDL_OP_DCL(p).raises_expr, idents);
		__idl_tree_free(IDL_OP_DCL(p).context_expr, idents);
		free(p);
		break;

	case IDLN_PARAM_DCL:
		__idl_tree_free(IDL_PARAM_DCL(p).param_type_spec, idents);
		__idl_tree_free(IDL_PARAM_DCL(p).simple_declarator, idents);
		free(p);
		break;
		
	case IDLN_FORWARD_DCL:
		__idl_tree_free(IDL_FORWARD_DCL(p).ident, idents);
		free(p);
		break;
		
	case IDLN_TYPE_STRING:
		__idl_tree_free(IDL_TYPE_STRING(p).positive_int_const, idents);
		free(p);
		break;
		
	case IDLN_TYPE_WIDE_STRING:
		__idl_tree_free(IDL_TYPE_WIDE_STRING(p).positive_int_const, idents);
		free(p);
		break;
		
	case IDLN_TYPE_FIXED:
		__idl_tree_free(IDL_TYPE_FIXED(p).positive_int_const, idents);
		__idl_tree_free(IDL_TYPE_FIXED(p).integer_lit, idents);
		free(p);
		break;

	case IDLN_TYPE_FLOAT:		
	case IDLN_TYPE_INTEGER:
	case IDLN_TYPE_CHAR:
	case IDLN_TYPE_WIDE_CHAR:
	case IDLN_TYPE_BOOLEAN:
	case IDLN_TYPE_OCTET:
	case IDLN_TYPE_ANY:
	case IDLN_TYPE_OBJECT:
		free(p);
		break;

	case IDLN_CASE_LABEL:
		__idl_tree_free(IDL_CASE_LABEL(p).const_exp, idents);
		free(p);
		break;
		
	case IDLN_INTERFACE:
		__idl_tree_free(IDL_INTERFACE(p).ident, idents);
		__idl_tree_free(IDL_INTERFACE(p).inheritence_spec, idents);
		__idl_tree_free(IDL_INTERFACE(p).body, idents);
		free(p);
		break;

	case IDLN_MODULE:
		__idl_tree_free(IDL_MODULE(p).ident, idents);
		__idl_tree_free(IDL_MODULE(p).definition_list, idents);
		free(p);
		break;

	case IDLN_BINOP:
		__idl_tree_free(IDL_BINOP(p).left, idents);
		__idl_tree_free(IDL_BINOP(p).right, idents);
		free(p);
		break;

	case IDLN_UNARYOP:
		__idl_tree_free(IDL_UNARYOP(p).operand, idents);
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

int IDL_parse_filename(const char *filename, const char *cpp_args,
		       IDL_callback cb, IDL_tree *tree, IDL_tree *symtab)
{
	extern void __idl_lex_init(void);
	extern void __idl_lex_cleanup(void);
	extern FILE *yyin;
	FILE *input;
	char *fmt = CPP_PROGRAM " %s %s";
	char *cmd;
	int rv;

	if (!filename || !tree) return -EINVAL;

	puts(cmd);
	cmd = (char *)malloc(strlen(filename) + 
			     (cpp_args ? strlen(cpp_args) : 0) +
			     strlen(fmt) - 4 + 1);
	if (!cmd)
		return -ENOMEM;

	sprintf(cmd, fmt, cpp_args ? cpp_args : "", filename);
	input = popen(cmd, "r");
	free(cmd);

	if (input == NULL)
		return errno;

	yyin = input;
	__idl_cb = cb;
	__idl_lex_init();
	rv = yyparse();
	__idl_lex_cleanup();
	__idl_cb = NULL;
	pclose(input);

	if (rv != 0)
		return IDL_ERROR;

	if (tree)
		*tree = __idl_root;
	else
		free(__idl_root);
	
	if (symtab)
		*symtab = __idl_symtab;
	else
		free(__idl_symtab);

	return IDL_SUCCESS;
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

static IDL_tree zlist_chain(IDL_tree a, IDL_tree b)
{
	if (a == NULL)
		return list_start(b);
	else
		return list_chain(a, b);
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

IDL_tree IDL_integer_new(long value)
{
	IDL_tree p = IDL_node_new(IDLN_INTEGER);

	IDL_INTEGER(p).value = value;

	return p;
}

IDL_tree IDL_string_new(char *value)
{
	IDL_tree p = IDL_node_new(IDLN_STRING);

	IDL_STRING(p).value = value;

	return p;
}

IDL_tree IDL_wide_string_new(wchar_t *value)
{
	IDL_tree p = IDL_node_new(IDLN_WIDE_STRING);

	IDL_WIDE_STRING(p).value = value;

	return p;
}

IDL_tree IDL_char_new(char *value)
{
	IDL_tree p = IDL_node_new(IDLN_CHAR);

	IDL_CHAR(p).value = value;

	return p;
}

IDL_tree IDL_wide_char_new(wchar_t *value)
{
	IDL_tree p = IDL_node_new(IDLN_WIDE_CHAR);

	IDL_WIDE_CHAR(p).value = value;

	return p;
}

IDL_tree IDL_fixed_new(double value)
{
	IDL_tree p = IDL_node_new(IDLN_FIXED);

	IDL_FIXED(p).value = value;

	return p;
}

IDL_tree IDL_float_new(double value)
{
	IDL_tree p = IDL_node_new(IDLN_FLOAT);

	IDL_FLOAT(p).value = value;

	return p;
}

IDL_tree IDL_boolean_new(unsigned value)
{
	IDL_tree p = IDL_node_new(IDLN_BOOLEAN);

	IDL_BOOLEAN(p).value = value;

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

IDL_tree IDL_type_float_new(enum IDL_float_type f_type)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_FLOAT);
	
	IDL_TYPE_FLOAT(p).f_type = f_type;

	return p;
}

IDL_tree IDL_type_fixed_new(IDL_tree positive_int_const,
			    IDL_tree integer_lit)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_FIXED);
	
	IDL_TYPE_FIXED(p).positive_int_const = positive_int_const;
	IDL_TYPE_FIXED(p).integer_lit = integer_lit;

	return p;
}


IDL_tree IDL_type_integer_new(unsigned f_signed, enum IDL_integer_type f_type)
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

IDL_tree IDL_type_string_new(IDL_tree positive_int_const)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_STRING);
	
	IDL_TYPE_STRING(p).positive_int_const = positive_int_const;

	return p;
}

IDL_tree IDL_type_wide_string_new(IDL_tree positive_int_const)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_WIDE_STRING);
	
	IDL_TYPE_WIDE_STRING(p).positive_int_const = positive_int_const;

	return p;
}

IDL_tree IDL_type_array_new(IDL_tree ident,
			    IDL_tree size_list)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_ARRAY);
	
	IDL_TYPE_ARRAY(p).ident = ident;
	IDL_TYPE_ARRAY(p).size_list = size_list;

	return p;
}

IDL_tree IDL_type_sequence_new(IDL_tree simple_type_spec,
			       IDL_tree positive_int_const)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_SEQUENCE);
	
	IDL_TYPE_SEQUENCE(p).simple_type_spec = simple_type_spec;
	IDL_TYPE_SEQUENCE(p).positive_int_const = positive_int_const;

	return p;
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

IDL_tree IDL_case_label_new(IDL_tree const_exp)
{
	IDL_tree p = IDL_node_new(IDLN_CASE_LABEL);
	
	IDL_CASE_LABEL(p).f_default = const_exp ? IDL_FALSE : IDL_TRUE;
	IDL_CASE_LABEL(p).const_exp = const_exp;

	return p;
}

IDL_tree IDL_interface_new(IDL_tree ident, IDL_tree inheritence_spec, IDL_tree body)
{
	IDL_tree p = IDL_node_new(IDLN_INTERFACE);
	
	IDL_INTERFACE(p).ident = ident;
	IDL_INTERFACE(p).inheritence_spec = inheritence_spec;
	IDL_INTERFACE(p).body = body;

	return p;
}

IDL_tree IDL_module_new(IDL_tree ident, IDL_tree definition_list)
{
	IDL_tree p = IDL_node_new(IDLN_MODULE);
	
	IDL_MODULE(p).ident = ident;
	IDL_MODULE(p).definition_list = definition_list;

	return p;
}

IDL_tree IDL_binop_new(enum IDL_binop op, IDL_tree left, IDL_tree right)
{
	IDL_tree p = IDL_node_new(IDLN_BINOP);
	
	IDL_BINOP(p).op = op;
	IDL_BINOP(p).left = left;
	IDL_BINOP(p).right = right;

	return p;
}

IDL_tree IDL_unaryop_new(enum IDL_unaryop op, IDL_tree operand)
{
	IDL_tree p = IDL_node_new(IDLN_UNARYOP);
	
	IDL_UNARYOP(p).op = op;
	IDL_UNARYOP(p).operand = operand;

	return p;
}

IDL_tree IDL_const_dcl_new(IDL_tree const_type, IDL_tree ident, IDL_tree const_exp)
{
	IDL_tree p = IDL_node_new(IDLN_CONST_DCL);
	
	IDL_CONST_DCL(p).const_type = const_type;
	IDL_CONST_DCL(p).ident = ident;
	IDL_CONST_DCL(p).const_exp = const_exp;

	return p;
}

IDL_tree IDL_except_dcl_new(IDL_tree ident, IDL_tree members)
{
	IDL_tree p = IDL_node_new(IDLN_EXCEPT_DCL);
	
	IDL_EXCEPT_DCL(p).ident = ident;
	IDL_EXCEPT_DCL(p).members = members;

	return p;
}

IDL_tree IDL_attr_dcl_new(unsigned f_readonly,
			  IDL_tree param_type_spec,
			  IDL_tree simple_declarations)
{
	IDL_tree p = IDL_node_new(IDLN_ATTR_DCL);
	
	IDL_ATTR_DCL(p).f_readonly = f_readonly;
	IDL_ATTR_DCL(p).param_type_spec = param_type_spec;
	IDL_ATTR_DCL(p).simple_declarations = simple_declarations;

	return p;
}

IDL_tree IDL_op_dcl_new(unsigned f_oneway,
			IDL_tree op_type_spec,
			IDL_tree ident,
			IDL_tree parameter_dcls,
			IDL_tree raises_expr,
			IDL_tree context_expr)
{
	IDL_tree p = IDL_node_new(IDLN_OP_DCL);
	
	IDL_OP_DCL(p).f_oneway = f_oneway;
	IDL_OP_DCL(p).op_type_spec = op_type_spec;
	IDL_OP_DCL(p).ident = ident;
	IDL_OP_DCL(p).parameter_dcls = parameter_dcls;
	IDL_OP_DCL(p).raises_expr = raises_expr;
	IDL_OP_DCL(p).context_expr = context_expr;

	return p;
}

IDL_tree IDL_param_dcl_new(enum IDL_param_attr attr,
			   IDL_tree param_type_spec,
			   IDL_tree simple_declarator)
{
	IDL_tree p = IDL_node_new(IDLN_PARAM_DCL);
	
	IDL_PARAM_DCL(p).attr = attr;
	IDL_PARAM_DCL(p).param_type_spec = param_type_spec;
	IDL_PARAM_DCL(p).simple_declarator = simple_declarator;

	return p;
}

IDL_tree IDL_forward_dcl_new(IDL_tree ident)
{
	IDL_tree p = IDL_node_new(IDLN_FORWARD_DCL);
	
	IDL_FORWARD_DCL(p).ident = ident;

	return p;
}
