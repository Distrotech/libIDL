/***************************************************************************

    parser.y (IDL yacc parser and tree generation)

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
#include <ctype.h>
#include <string.h>
#include "rename.h"
#include "util.h"
#include "IDL.h"

#define do_binop(rv,op,a,b)	do {		\
	if (IDL_binop_chktypes(op, a, b))	\
		YYABORT;			\
	if (flags & IDLF_EVAL_CONST) {		\
		rv = IDL_binop_eval(op, a, b);	\
		__IDL_tree_free(a);		\
		__IDL_tree_free(b);		\
		if (!rv) YYABORT;		\
	} else {				\
		rv = IDL_binop_new(op, a, b);	\
	}					\
} while (0)

#define do_unaryop(rv,op,a)	do {		\
	if (IDL_unaryop_chktypes(op, a))	\
		YYABORT;			\
	if (flags & IDLF_EVAL_CONST) {		\
		rv = IDL_unaryop_eval(op, a);	\
		__IDL_tree_free(a);		\
		if (!rv) YYABORT;		\
	} else {				\
		rv = IDL_unaryop_new(op, a);	\
	}					\
} while (0)

extern int			yylex(void);

void				__IDL_tree_print(IDL_tree p);
static int			IDL_binop_chktypes(enum IDL_binop op,
						   IDL_tree a,
						   IDL_tree b);
static int			IDL_unaryop_chktypes(enum IDL_unaryop op,
						     IDL_tree a);
static void			__IDL_tree_free(IDL_tree p);
static IDL_tree			IDL_binop_eval(enum IDL_binop op,
					       IDL_tree a,
					       IDL_tree b);
static IDL_tree			IDL_unaryop_eval(enum IDL_unaryop op,
						 IDL_tree a);
static void			assign_up_node(IDL_tree up, IDL_tree node);
static IDL_tree			list_start(IDL_tree a);
static IDL_tree			list_chain(IDL_tree a, IDL_tree b);
static IDL_tree			zlist_chain(IDL_tree a, IDL_tree b);

char *				__IDL_cur_filename = NULL;
int				__IDL_cur_line;
static unsigned long		flags;
static int			idl_nerrors, idl_nwarnings;
static IDL_tree			idl_root;
static IDL_ns			idl_ns;
static IDL_callback		idl_msgcb;
static int			idl_is_parsing = IDL_FALSE;
%}

%union {
	IDL_tree tree;
	char *str;
	IDL_long_t integer;
	double floatp;
	enum IDL_unaryop unaryop;
	enum IDL_param_attr paramattr;
}

%token				TOK_ANY TOK_ATTRIBUTE TOK_BOOLEAN TOK_CASE TOK_CHAR
%token				TOK_CONST TOK_CONTEXT TOK_DEFAULT TOK_DOUBLE TOK_ENUM
%token				TOK_EXCEPTION TOK_FALSE TOK_FIXED TOK_FLOAT TOK_IN 
%token				TOK_INOUT TOK_INTERFACE TOK_LONG TOK_MODULE TOK_NATIVE TOK_OBJECT
%token				TOK_OCTET TOK_ONEWAY TOK_OUT TOK_RAISES TOK_READONLY 
%token				TOK_SEQUENCE TOK_SHORT TOK_STRING TOK_STRUCT TOK_SWITCH
%token				TOK_TRUE TOK_TYPEDEF TOK_UNSIGNED TOK_UNION TOK_VOID
%token				TOK_WCHAR TOK_WSTRING TOK_OP_SCOPE TOK_OP_SHR TOK_OP_SHL

%token <str>			TOK_IDENT TOK_SQSTRING TOK_DQSTRING
%token <str>			TOK_FIXEDP
%token <integer>		TOK_INTEGER
%token <floatp>			TOK_FLOATP

%type <tree>			specification
%type <tree>			definition_list definition
%type <tree>			new_scope new_or_prev_scope pop_scope
%type <tree>			type_dcl const_dcl except_dcl attr_dcl op_dcl
%type <tree>			type_spec type_declarator declarator_list declarator
%type <tree>			simple_type_spec constr_type_spec base_type_spec
%type <tree>			template_type_spec sequence_type array_declarator
%type <tree>			simple_declarator complex_declarator simple_declarator_list
%type <tree>			member member_list member_zlist enumerator_list
%type <tree>			switch_type_spec switch_body struct_type union_type
%type <tree>			interface interface_dcl forward_dcl scoped_name_list
%type <tree>			interface_body z_inheritance export_list export module
%type <tree>			const_type new_ident prev_ident new_or_prev_ident 
%type <tree>			global_ident ident floating_pt_type integer_type
%type <tree>			char_type wide_char_type boolean_type octet_type
%type <tree>			string_type wide_string_type fixed_pt_type fixed_pt_const_type
%type <tree>			any_type object_type enum_type scoped_name
%type <tree>			case_stmt case_label case_label_list fixed_array_size_list
%type <tree>			case_stmt_list fixed_array_size positive_int_const
%type <tree>			ns_scoped_name ns_prev_ident ns_new_ident ns_global_ident
%type <tree>			ns_new_or_prev_ident cur_ns_new_or_prev_ident
%type <tree>			param_type_spec op_type_spec parameter_dcls
%type <tree>			is_raises_expr is_context_expr param_dcl_list
%type <tree>			param_dcl raises_expr context_expr element_spec
%type <tree>			const_exp or_expr xor_expr and_expr shift_expr
%type <tree>			add_expr mult_expr unary_expr primary_expr literal
%type <tree>			integer_lit
%type <tree>			string_lit char_lit /* wide_string_lit wide_char_lit */ 
%type <tree>			fixed_pt_lit floating_pt_lit
%type <tree>			boolean_lit
%type <tree>			string_lit_list
%type <str>			sqstring dqstring dqstring_cat
%type <integer>			signed_int unsigned_int is_readonly is_oneway
%type <unaryop>			unary_op
%type <paramattr>		param_attribute

%%

start:			idl_init
			specification
			idl_finish			{
	idl_root = NULL;
	idl_root = $2;
}
	;

idl_init:		/* empty */			{
	idl_nerrors = idl_nwarnings = 0;
}
	;

idl_finish:		/* empty */
	;

specification:		/* empty */			{ yyerror("file is empty"); YYABORT; }
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

module:			TOK_MODULE new_or_prev_scope '{'
				definition_list
			'}' pop_scope			{ $$ = IDL_module_new($2, $4); }
	;

interface_dcl:		TOK_INTERFACE new_or_prev_scope z_inheritance '{'
				interface_body
			'}' pop_scope			{ $$ = IDL_interface_new($2, $3, $5); }
	;

forward_dcl:		TOK_INTERFACE
			new_or_prev_scope pop_scope	{ $$ = IDL_forward_dcl_new($2); }
	;

z_inheritance:		/* empty */			{ $$ = NULL; }
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

type_dcl:		TOK_TYPEDEF type_declarator	{ $$ = $2; }
|			struct_type
|			union_type
|			enum_type
|			TOK_NATIVE simple_declarator	{ $$ = $2; }
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

struct_type:		TOK_STRUCT new_scope '{'
				member_list
			'}' pop_scope			{ $$ = IDL_type_struct_new($2, $4); }
	;

union_type:		TOK_UNION new_scope TOK_SWITCH '('
				switch_type_spec
			')' '{'
				switch_body
			'}' pop_scope			{ $$ = IDL_type_union_new($2, $5, $8); }
	;

switch_type_spec:	integer_type
|			char_type
|			boolean_type
|			enum_type
|			scoped_name
	;

switch_body:		case_stmt_list
	;

case_stmt_list:		case_stmt			{ $$ = list_start($1); }
|			case_stmt_list case_stmt	{ $$ = list_chain($1, $2); }
	;

case_stmt:		case_label_list
			element_spec ';'		{ $$ = IDL_case_stmt_new($1, $2); }
	;

element_spec:		type_spec declarator		{ $$ = IDL_member_new($1, list_start($2)); }
	;

case_label_list:	case_label			{ $$ = list_start($1); }
|			case_label_list case_label	{ $$ = list_chain($1, $2); }
	;

case_label:		TOK_CASE const_exp ':'		{ $$ = $2; }
|			TOK_DEFAULT ':'			{ $$ = NULL; }
	;

const_dcl:		TOK_CONST const_type new_ident
			'=' const_exp			{ $$ = IDL_const_dcl_new($2, $3, $5); }
	;

except_dcl:		TOK_EXCEPTION new_scope '{'
				member_zlist
			'}' pop_scope			{ $$ = IDL_except_dcl_new($2, $4); }
	;

member_zlist:		/* empty */			{ $$ = NULL; }
|			member_zlist member		{ $$ = zlist_chain($1, $2); }
	;

is_readonly:		/* empty */			{ $$ = IDL_FALSE; }
|			TOK_READONLY			{ $$ = IDL_TRUE; }
	;

attr_dcl:		is_readonly TOK_ATTRIBUTE
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
|			TOK_ONEWAY			{ $$ = IDL_TRUE; }
	;

op_dcl:			is_oneway op_type_spec
			new_scope parameter_dcls pop_scope
			is_raises_expr
			is_context_expr			{ $$ = IDL_op_dcl_new($1, $2, $3, $4, $6, $7); }
	;

op_type_spec:		param_type_spec
|			TOK_VOID			{ $$ = NULL; }
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

param_attribute:	TOK_IN				{ $$ = IDL_PARAM_IN; }
|			TOK_OUT				{ $$ = IDL_PARAM_OUT; }
|			TOK_INOUT			{ $$ = IDL_PARAM_INOUT; }
	;

is_raises_expr:		/* empty */			{ $$ = NULL; }
|			raises_expr
	;

is_context_expr:	/* empty */			{ $$ = NULL; }
|			context_expr
	;

raises_expr:		TOK_RAISES '('
				scoped_name_list
			')'				{ $$ = $3; }
	;

context_expr:		TOK_CONTEXT '('
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
|			or_expr '|' xor_expr		{ do_binop($$, IDL_BINOP_OR, $1, $3); }
	;

xor_expr:		and_expr
|			xor_expr '^' and_expr		{ do_binop($$, IDL_BINOP_XOR, $1, $3); }
	;

and_expr:		shift_expr
|			and_expr '&' shift_expr		{ do_binop($$, IDL_BINOP_AND, $1, $3); }
	;

shift_expr:		add_expr
|			shift_expr TOK_OP_SHR add_expr	{ do_binop($$, IDL_BINOP_SHR, $1, $3); }
|			shift_expr TOK_OP_SHL add_expr	{ do_binop($$, IDL_BINOP_SHL, $1, $3); }
	;

add_expr:		mult_expr
|			add_expr '+' mult_expr		{ do_binop($$, IDL_BINOP_ADD, $1, $3); }
|			add_expr '-' mult_expr		{ do_binop($$, IDL_BINOP_SUB, $1, $3); }
	;

mult_expr:		unary_expr
|			mult_expr '*' unary_expr	{ do_binop($$, IDL_BINOP_MULT, $1, $3); }
|			mult_expr '/' unary_expr	{ do_binop($$, IDL_BINOP_DIV, $1, $3); }
|			mult_expr '%' unary_expr	{ do_binop($$, IDL_BINOP_MOD, $1, $3); }
	;

unary_expr:		unary_op primary_expr		{ do_unaryop($$, $1, $2); }
|			primary_expr
	;

unary_op:		'-'				{ $$ = IDL_UNARYOP_MINUS; }
|			'+'				{ $$ = IDL_UNARYOP_PLUS; }
|			'~'				{ $$ = IDL_UNARYOP_COMPLEMENT; }
	;

primary_expr:		scoped_name
|			literal				{
	switch (IDL_NODE_TYPE($1)) {
	case IDLN_INTEGER:
	case IDLN_FLOAT:
	case IDLN_FIXED:
	case IDLN_CHAR:
	case IDLN_WIDE_CHAR:
		break;
	default:
		yyerror("illegal type in constant expression");
		YYABORT;
		break;
	}
	$$ = $1;
}
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

enum_type:		TOK_ENUM new_scope '{'
				enumerator_list
			'}' pop_scope			{ $$ = IDL_type_enum_new($2, $4); }
	;

scoped_name:		ns_scoped_name			{
	assert($1 != NULL);
	assert(IDL_NODE_TYPE($1) == IDLN_GENTREE);
	assert(IDL_NODE_TYPE(IDL_GENTREE($1).data) == IDLN_IDENT);
	$$ = IDL_GENTREE($1).data;
}
	;

ns_scoped_name:		ns_prev_ident
|			TOK_OP_SCOPE ns_global_ident		{ $$ = $2; }
|			ns_scoped_name TOK_OP_SCOPE
			ident				{
	IDL_tree p;

	assert(IDL_NODE_TYPE($1) == IDLN_GENTREE);
	assert(IDL_NODE_TYPE($3) == IDLN_IDENT);

#ifdef YYDEBUG
	if (yydebug)
		printf("looking in %s\n", IDL_IDENT(IDL_GENTREE($1).data).str);
#endif

	if ((p = IDL_ns_lookup_this_scope(idl_ns, $1, $3)) == NULL) {
#ifdef YYDEBUG
		if (yydebug)
			printf("'%s'\n", IDL_IDENT($3).str);
#endif
		IDL_tree_free($3);
		yyerror("undeclared identifier");
		YYABORT;
	}
	IDL_tree_free($3);
	++IDL_IDENT(IDL_GENTREE(p).data)._refs;
	$$ = p;
}
	;

enumerator_list:	new_ident			{ $$ = list_start($1); }
|			enumerator_list ',' new_ident	{ $$ = list_chain($1, $3); }
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

sequence_type:		TOK_SEQUENCE '<'
				simple_type_spec ',' positive_int_const
			'>'				{ $$ = IDL_type_sequence_new($3, $5); }
|			TOK_SEQUENCE '<'
				simple_type_spec
			'>'				{ $$ = IDL_type_sequence_new($3, NULL); }
	;

floating_pt_type:	TOK_FLOAT			{ $$ = IDL_type_float_new(IDL_FLOAT_TYPE_FLOAT); }
|			TOK_DOUBLE			{ $$ = IDL_type_float_new(IDL_FLOAT_TYPE_DOUBLE); }
|			TOK_LONG TOK_DOUBLE		{ $$ = IDL_type_float_new(IDL_FLOAT_TYPE_LONGDOUBLE); }
	;

fixed_pt_type:		TOK_FIXED '<'
				positive_int_const ',' integer_lit
			'>'				{ $$ = IDL_type_fixed_new($3, $5); }
	;

fixed_pt_const_type:	TOK_FIXED			{ $$ = IDL_type_fixed_new(NULL, NULL); }
	;

integer_type:		signed_int			{ $$ = IDL_type_integer_new(IDL_TRUE, $1); }
|			unsigned_int			{ $$ = IDL_type_integer_new(IDL_FALSE, $1); }
	;

signed_int:		signed_short_int		{ $$ = IDL_INTEGER_TYPE_SHORT; }
|			signed_long_int			{ $$ = IDL_INTEGER_TYPE_LONG; }
|			signed_longlong_int		{ $$ = IDL_INTEGER_TYPE_LONGLONG; }
	;

signed_short_int:	TOK_SHORT
	;

signed_long_int:	TOK_LONG
	;

signed_longlong_int:	TOK_LONG TOK_LONG
	;

unsigned_int:		unsigned_short_int		{ $$ = IDL_INTEGER_TYPE_SHORT; }
|			unsigned_long_int		{ $$ = IDL_INTEGER_TYPE_LONG; }
|			unsigned_longlong_int		{ $$ = IDL_INTEGER_TYPE_LONGLONG; }
	;

unsigned_short_int:	TOK_UNSIGNED TOK_SHORT
	;

unsigned_long_int:	TOK_UNSIGNED TOK_LONG
	;

unsigned_longlong_int:	TOK_UNSIGNED TOK_LONG TOK_LONG
	;

char_type:		TOK_CHAR			{ $$ = IDL_type_char_new(); }
	;

wide_char_type:		TOK_WCHAR			{ $$ = IDL_type_wide_char_new(); }
	;

boolean_type:		TOK_BOOLEAN			{ $$ = IDL_type_boolean_new(); }
	;

octet_type:		TOK_OCTET			{ $$ = IDL_type_octet_new(); }
	;

any_type:		TOK_ANY				{ $$ = IDL_type_any_new(); }
	;

object_type:		TOK_OBJECT			{ $$ = IDL_type_object_new(); }
	;

string_type:		TOK_STRING '<'
				positive_int_const
				'>'			{ $$ = IDL_type_string_new($3); }
|			TOK_STRING			{ $$ = IDL_type_string_new(NULL); }
	;

wide_string_type:	TOK_WSTRING '<'
				positive_int_const
			'>'				{ $$ = IDL_type_wide_string_new($3); }
|			TOK_WSTRING			{ $$ = IDL_type_wide_string_new(NULL); }
	;

declarator_list:	declarator			{ $$ = list_start($1); }
|			declarator_list 
			',' declarator			{ $$ = list_chain($1, $3); }
	;

declarator:		simple_declarator
|			complex_declarator
	;

simple_declarator:	new_ident
	;

complex_declarator:	array_declarator
	;

simple_declarator_list:	simple_declarator		{ $$ = list_start($1); }
|			simple_declarator_list
			simple_declarator		{ $$ = list_chain($1, $2); }
	;

array_declarator:	new_ident
			fixed_array_size_list		{ $$ = IDL_type_array_new($1, $2); }
	;

fixed_array_size_list:	fixed_array_size		{ $$ = list_start($1); }
|			fixed_array_size_list
			fixed_array_size		{ $$ = list_chain($1, $2); }
	;

fixed_array_size:	'[' 
			positive_int_const 
			']'				{ $$ = $2; }
	;

ident:			TOK_IDENT			{ $$ = IDL_ident_new($1); }
	;

new_ident:		ns_new_ident			{
	assert($1 != NULL);
	assert(IDL_NODE_TYPE($1) == IDLN_GENTREE);
	assert(IDL_NODE_TYPE(IDL_GENTREE($1).data) == IDLN_IDENT);
	$$ = IDL_GENTREE($1).data;
}
	;

prev_ident:		ns_prev_ident			{
	assert($1 != NULL);
	assert(IDL_NODE_TYPE($1) == IDLN_GENTREE);
	assert(IDL_NODE_TYPE(IDL_GENTREE($1).data) == IDLN_IDENT);
	$$ = IDL_GENTREE($1).data;
}
	;

new_or_prev_ident:	ns_new_or_prev_ident		{
	assert($1 != NULL);
	assert(IDL_NODE_TYPE($1) == IDLN_GENTREE);
	assert(IDL_NODE_TYPE(IDL_GENTREE($1).data) == IDLN_IDENT);
	$$ = IDL_GENTREE($1).data;
}
	;

global_ident:		ns_global_ident			{
	assert($1 != NULL);
	assert(IDL_NODE_TYPE($1) == IDLN_GENTREE);
	assert(IDL_NODE_TYPE(IDL_GENTREE($1).data) == IDLN_IDENT);
	$$ = IDL_GENTREE($1).data;
}
	;

new_scope:		ns_new_ident			{
	IDL_ns_push_scope(idl_ns, $1);
#ifdef YYDEBUG
	if (yydebug)
		printf("entering new/prev scope of %s\n", 
		       IDL_IDENT(IDL_GENTREE(IDL_NS(idl_ns).current).data).str);
#endif
	assert(IDL_NODE_TYPE(IDL_GENTREE($1).data) == IDLN_IDENT);
	$$ = IDL_GENTREE($1).data;
}
	;

new_or_prev_scope:	cur_ns_new_or_prev_ident	{
	IDL_ns_push_scope(idl_ns, $1);
	assert(IDL_NS(idl_ns).current != NULL);
	assert(IDL_NODE_TYPE(IDL_NS(idl_ns).current) == IDLN_GENTREE);
	assert(IDL_GENTREE(IDL_NS(idl_ns).current).data != NULL);
	assert(IDL_NODE_TYPE(IDL_GENTREE(IDL_NS(idl_ns).current).data) == IDLN_IDENT);
#ifdef YYDEBUG
	if (yydebug)
		printf("entering new/prev scope of %s\n", 
		       IDL_IDENT(IDL_GENTREE(IDL_NS(idl_ns).current).data).str);
#endif
	assert(IDL_NODE_TYPE(IDL_GENTREE($1).data) == IDLN_IDENT);
	$$ = IDL_GENTREE($1).data;
}
	;

pop_scope:		/* empty */			{
#ifdef YYDEBUG
	if (yydebug)
		printf("scope to parent of %s\n", 
		       IDL_IDENT(IDL_GENTREE(IDL_NS(idl_ns).current).data).str);
#endif
	IDL_ns_pop_scope(idl_ns);
}
	;

ns_new_ident:		ident				{
	IDL_tree p;

	if ((p = IDL_ns_place_new(idl_ns, $1)) == NULL) {
		IDL_tree_free($1);
		yyerror("duplicate identifier");
		YYABORT;
	}
	assert(IDL_IDENT($1)._ns_ref == p);
	++IDL_IDENT(IDL_GENTREE(p).data)._refs;
	$$ = p;
}
	;

ns_prev_ident:		ident				{
	IDL_tree p;

	if ((p = IDL_ns_resolve_ident(idl_ns, $1)) == NULL) {
		IDL_tree_free($1);
		yyerror("undeclared identifier");
		YYABORT;
	}
	IDL_tree_free($1);
	assert(IDL_GENTREE(p).data != NULL);
	assert(IDL_IDENT(IDL_GENTREE(p).data)._ns_ref == p);
	++IDL_IDENT(IDL_GENTREE(p).data)._refs;
	$$ = p;
}
	;

ns_new_or_prev_ident:	ident				{
	IDL_tree p;

	if ((p = IDL_ns_resolve_ident(idl_ns, $1)) == NULL) {
		p = IDL_ns_place_new(idl_ns, $1);
		assert(p != NULL);
		assert(IDL_IDENT($1)._ns_ref == p);
	} else {
		IDL_tree_free($1);
		assert(IDL_GENTREE(p).data != NULL);
		assert(IDL_IDENT(IDL_GENTREE(p).data)._ns_ref == p);
	}
	++IDL_IDENT(IDL_GENTREE(p).data)._refs;
	$$ = p;
}
	;

cur_ns_new_or_prev_ident:
			ident				{
	IDL_tree p;

	if ((p = IDL_ns_lookup_cur_scope(idl_ns, $1)) == NULL) {
		p = IDL_ns_place_new(idl_ns, $1);
		assert(p != NULL);
		assert(IDL_IDENT($1)._ns_ref == p);
	} else {
		IDL_tree_free($1);
		assert(IDL_GENTREE(p).data != NULL);
		assert(IDL_IDENT(IDL_GENTREE(p).data)._ns_ref == p);
	}
	++IDL_IDENT(IDL_GENTREE(p).data)._refs;
	$$ = p;
}
	;

ns_global_ident:	ident				{
	IDL_tree p;

	if ((p = IDL_ns_lookup_this_scope(idl_ns, IDL_NS(idl_ns).file, $1)) == NULL) {
		IDL_tree_free($1);
		yyerror("undeclared identifier");
		YYABORT;
	}
	IDL_tree_free($1);
	assert(IDL_GENTREE(p).data != NULL);
	assert(IDL_IDENT(IDL_GENTREE(p).data)._ns_ref == p);
	++IDL_IDENT(IDL_GENTREE(p).data)._refs;
	$$ = p;
}
	;

string_lit_list:	string_lit			{ $$ = list_start($1); }
|			string_lit_list ',' 
			string_lit			{ $$ = list_chain($1, $3); }
	;

positive_int_const:	TOK_INTEGER			{
	if ($1 < 0) {
		yywarning("cannot use negative value, using absolute value");
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

boolean_lit:		TOK_TRUE			{ $$ = IDL_boolean_new(IDL_TRUE); }
|			TOK_FALSE			{ $$ = IDL_boolean_new(IDL_FALSE); }
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
	char *s = IDL_do_escapes($1);
	free($1);
	$$ = s;
}
	;

sqstring:		TOK_SQSTRING			{
	char *s = IDL_do_escapes($1);
	free($1);
	$$ = s;
}
	;

%%

void yyerrorl(const char *s, int ofs)
{
	int line = __IDL_cur_line - 1 + ofs;

	++idl_nerrors;
	
	if (idl_msgcb)
		(*idl_msgcb)(IDL_ERROR, idl_nerrors, line,
			     __IDL_cur_filename, s);
	else
		fprintf(stderr, "%s:%d: %s\n", 
			__IDL_cur_filename, line, s);
}

void yywarningl(const char *s, int ofs)
{
	int line = __IDL_cur_line - 1 + ofs;
	
	++idl_nwarnings;
	
	if (idl_msgcb)
		(*idl_msgcb)(IDL_WARNING, idl_nwarnings, line,
			     __IDL_cur_filename, s);
	else
		fprintf(stderr, "%s:%d: warning: %s\n",
			__IDL_cur_filename, line, s);
}

void yyerror(const char *s)
{
	yyerrorl(s, 0);
}

void yywarning(const char *s)
{
	yywarningl(s, 0);
}

const char *IDL_get_libver_string(void)
{
	return VERSION;
}

const char *IDL_get_IDLver_string(void)
{
	return "2.2";
}

void __IDL_do_pragma(const char *s)
{
	int n;
	char directive[256];

	if (!s)
		return;

	if (sscanf(s, "%255s%n", directive, &n) < 1)
		return;
	s += n;
	while (isspace(*s)) ++s;

	if (strcmp(directive, "prefix") == 0)
		IDL_ns_prefix(idl_ns, s);
}

#define C_ESC(a,b)				case a: *p++ = b; ++s; break
char *IDL_do_escapes(const char *s)
{
	char *p, *q;

	if (!s)
		return NULL;

	p = q = (char *)malloc(strlen(s) + 1);
	
	while (*s) {
		if (*s != '\\') {
			*p++ = *s++;
			continue;
		}
		++s;		
		if (*s == 'x') {
			char hex[3];
			int n;
			hex[0] = 0;
			++s;
			sscanf(s, "%2[0-9a-fA-F]", hex);
 			s += strlen(hex);
			sscanf(hex, "%x", &n);
			*p++ = n;
			continue;
		}
		if (*s >= '0' && *s <= '7') {
			char oct[4];
			int n;
			oct[0] = 0;
			sscanf(s, "%3[0-7]", oct);
 			s += strlen(oct);
			sscanf(oct, "%o", &n);
			*p++ = n;
			continue;
		}
		switch (*s) {
			C_ESC('n','\n');
			C_ESC('t','\t');
			C_ESC('v','\v');
			C_ESC('b','\b');
			C_ESC('r','\r');
			C_ESC('f','\f');
			C_ESC('a','\a');
			C_ESC('\\','\\');
			C_ESC('?','?');
			C_ESC('\'','\'');
			C_ESC('"','"');
		}
	}
	*p = 0;

	return q;
}

int
IDL_list_length(IDL_tree list)
{
	int retval;
	IDL_tree curitem;
	for(curitem = list, retval = 0; curitem;
	    curitem = IDL_LIST(curitem).next)
		retval++;

	return retval;
}

void __IDL_tree_print(IDL_tree p)
{
	IDL_tree q, r;

	if (p == NULL)
		return;

	switch (IDL_NODE_TYPE(p)) {
	case IDLN_LIST:
		printf("IDL list\n");
		while (p) {
			__IDL_tree_print(IDL_LIST(p).data);
			p = IDL_LIST(p).next;
		}
		break;

	case IDLN_GENTREE:
		__IDL_tree_print(IDL_GENTREE(p).data);
		__IDL_tree_print(IDL_GENTREE(p).children);

		q = IDL_GENTREE(p).siblings;
		while (q != NULL) {
			r = IDL_GENTREE(q).siblings;
			__IDL_tree_print(IDL_GENTREE(q).data);
			__IDL_tree_print(IDL_GENTREE(q).children);
			q = r;
		}
		break;

	case IDLN_INTEGER:
		printf("IDL integer: %ld\n", IDL_INTEGER(p).value);
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
		__IDL_tree_print(IDL_MEMBER(p).type_spec);
		__IDL_tree_print(IDL_MEMBER(p).dcls);
		break;
		
	case IDLN_TYPE_DCL:
		printf("IDL type declaration\n");
		__IDL_tree_print(IDL_TYPE_DCL(p).type_spec);
		__IDL_tree_print(IDL_TYPE_DCL(p).dcls);
		break;

	case IDLN_CONST_DCL:
		printf("IDL const declaration\n");
		__IDL_tree_print(IDL_CONST_DCL(p).const_type);
		__IDL_tree_print(IDL_CONST_DCL(p).ident);
		__IDL_tree_print(IDL_CONST_DCL(p).const_exp);
		break;
		
	case IDLN_EXCEPT_DCL:
		printf("IDL exception declaration\n");
		__IDL_tree_print(IDL_EXCEPT_DCL(p).ident);
		__IDL_tree_print(IDL_EXCEPT_DCL(p).members);
		break;
		
	case IDLN_ATTR_DCL:
		printf("IDL attr declaration\n");
		__IDL_tree_print(IDL_ATTR_DCL(p).param_type_spec);
		__IDL_tree_print(IDL_ATTR_DCL(p).simple_declarations);
		break;
		
	case IDLN_OP_DCL:
		printf("IDL op declaration\n");
		__IDL_tree_print(IDL_OP_DCL(p).op_type_spec);
		__IDL_tree_print(IDL_OP_DCL(p).ident);
		__IDL_tree_print(IDL_OP_DCL(p).parameter_dcls);
		__IDL_tree_print(IDL_OP_DCL(p).raises_expr);
		__IDL_tree_print(IDL_OP_DCL(p).context_expr);
		break;

	case IDLN_PARAM_DCL:
		printf("IDL param declaration: %d\n", IDL_PARAM_DCL(p).attr);
		__IDL_tree_print(IDL_PARAM_DCL(p).param_type_spec);
		__IDL_tree_print(IDL_PARAM_DCL(p).simple_declarator);
		break;

	case IDLN_FORWARD_DCL:
		printf("IDL forward declaration\n");
		__IDL_tree_print(IDL_FORWARD_DCL(p).ident);
		break;
		
	case IDLN_TYPE_FLOAT:
		printf("IDL float type: %d\n", IDL_TYPE_FLOAT(p).f_type);
		break;

	case IDLN_TYPE_FIXED:
		printf("IDL fixed type\n");
		__IDL_tree_print(IDL_TYPE_FIXED(p).positive_int_const);
		__IDL_tree_print(IDL_TYPE_FIXED(p).integer_lit);
		break;

	case IDLN_TYPE_INTEGER:
		printf("IDL integer type: %d %d\n",
		       IDL_TYPE_INTEGER(p).f_signed,
		       IDL_TYPE_INTEGER(p).f_type);
		break;

	case IDLN_TYPE_STRING:
		printf("IDL string type\n");
		__IDL_tree_print(IDL_TYPE_STRING(p).positive_int_const);
		break;

	case IDLN_TYPE_WIDE_STRING:
		printf("IDL wide string type\n");
		__IDL_tree_print(IDL_TYPE_WIDE_STRING(p).positive_int_const);
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
		__IDL_tree_print(IDL_TYPE_ENUM(p).ident);
		__IDL_tree_print(IDL_TYPE_ENUM(p).enumerator_list);
		break;

	case IDLN_TYPE_SEQUENCE:
		printf("IDL sequence type\n");
		__IDL_tree_print(IDL_TYPE_SEQUENCE(p).simple_type_spec);
		__IDL_tree_print(IDL_TYPE_SEQUENCE(p).positive_int_const);
		break;

	case IDLN_TYPE_ARRAY:
		printf("IDL array type\n");
		__IDL_tree_print(IDL_TYPE_ARRAY(p).ident);
		__IDL_tree_print(IDL_TYPE_ARRAY(p).size_list);
		break;

	case IDLN_TYPE_STRUCT:
		printf("IDL struct type\n");
		__IDL_tree_print(IDL_TYPE_STRUCT(p).ident);
		__IDL_tree_print(IDL_TYPE_STRUCT(p).member_list);
		break;
		
	case IDLN_TYPE_UNION:
		printf("IDL union type\n");
		__IDL_tree_print(IDL_TYPE_UNION(p).ident);
		__IDL_tree_print(IDL_TYPE_UNION(p).switch_type_spec);
		__IDL_tree_print(IDL_TYPE_UNION(p).switch_body);
		break;

	case IDLN_CASE_STMT:
		__IDL_tree_print(IDL_CASE_STMT(p).labels);
		__IDL_tree_print(IDL_CASE_STMT(p).element_spec);
		break;

	case IDLN_INTERFACE:
		__IDL_tree_print(IDL_INTERFACE(p).ident);
		__IDL_tree_print(IDL_INTERFACE(p).inheritance_spec);
		__IDL_tree_print(IDL_INTERFACE(p).body);
		break;

	case IDLN_MODULE:
		__IDL_tree_print(IDL_MODULE(p).ident);
		__IDL_tree_print(IDL_MODULE(p).definition_list);
		break;		

	case IDLN_BINOP:
		printf("IDL binop: op %d\n", IDL_BINOP(p).op);
		__IDL_tree_print(IDL_BINOP(p).left);
		__IDL_tree_print(IDL_BINOP(p).right);
		break;

	case IDLN_UNARYOP:
		printf("IDL unary: op %d\n", IDL_UNARYOP(p).op);
		__IDL_tree_print(IDL_UNARYOP(p).operand);
		break;
		
	default:
		fprintf(stderr, "warning: print unknown node: %d\n", IDL_NODE_TYPE(p));
		break;
	}
}

static void __IDL_tree_free(IDL_tree p)
{
	IDL_tree q, r;

	if (!p)
		return;

	switch (IDL_NODE_TYPE(p)) {
	case IDLN_LIST:
		while (p) {
			__IDL_tree_free(IDL_LIST(p).data);
			q = IDL_LIST(p).next;
			free(p);
			p = q;
		}
		break;

	case IDLN_GENTREE:
		__IDL_tree_free(IDL_GENTREE(p).data);
		__IDL_tree_free(IDL_GENTREE(p).children);

		q = IDL_GENTREE(p).siblings;
		free(p);
		while (q != NULL) {
			r = IDL_GENTREE(q).siblings;
			__IDL_tree_free(IDL_GENTREE(q).data);
			__IDL_tree_free(IDL_GENTREE(q).children);
			free(q);
			q = r;
		}
		break;

	case IDLN_FIXED:
		free(IDL_FIXED(p).value);
		free(p);
		break;

	case IDLN_INTEGER:
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
		if (--IDL_IDENT(p)._refs <= 0) {
			free(IDL_IDENT(p).str);
			free(p);
		}
		break;

	case IDLN_MEMBER:
		__IDL_tree_free(IDL_MEMBER(p).type_spec);
		__IDL_tree_free(IDL_MEMBER(p).dcls);
		free(p);
		break;

	case IDLN_TYPE_ENUM:
		__IDL_tree_free(IDL_TYPE_ENUM(p).ident);
		__IDL_tree_free(IDL_TYPE_ENUM(p).enumerator_list);
		free(p);
		break;

	case IDLN_TYPE_SEQUENCE:
		__IDL_tree_free(IDL_TYPE_SEQUENCE(p).simple_type_spec);
		__IDL_tree_free(IDL_TYPE_SEQUENCE(p).positive_int_const);
		free(p);
		break;

	case IDLN_TYPE_ARRAY:
		__IDL_tree_free(IDL_TYPE_ARRAY(p).ident);
		__IDL_tree_free(IDL_TYPE_ARRAY(p).size_list);
		free(p);
		break;

	case IDLN_TYPE_STRUCT:
		__IDL_tree_free(IDL_TYPE_STRUCT(p).ident);
		__IDL_tree_free(IDL_TYPE_STRUCT(p).member_list);
		free(p);
		break;

	case IDLN_TYPE_UNION:
		__IDL_tree_free(IDL_TYPE_UNION(p).ident);
		__IDL_tree_free(IDL_TYPE_UNION(p).switch_type_spec);
		__IDL_tree_free(IDL_TYPE_UNION(p).switch_body);
		free(p);
		break;
				
	case IDLN_TYPE_DCL:
		__IDL_tree_free(IDL_TYPE_DCL(p).type_spec);
		__IDL_tree_free(IDL_TYPE_DCL(p).dcls);
		free(p);
		break;

	case IDLN_CONST_DCL:
		__IDL_tree_free(IDL_CONST_DCL(p).const_type);
		__IDL_tree_free(IDL_CONST_DCL(p).ident);
		__IDL_tree_free(IDL_CONST_DCL(p).const_exp);
		free(p);
		break;

	case IDLN_EXCEPT_DCL:
		__IDL_tree_free(IDL_EXCEPT_DCL(p).ident);
		__IDL_tree_free(IDL_EXCEPT_DCL(p).members);
		free(p);
		break;
		
	case IDLN_ATTR_DCL:
		__IDL_tree_free(IDL_ATTR_DCL(p).param_type_spec);
		__IDL_tree_free(IDL_ATTR_DCL(p).simple_declarations);
		free(p);
		break;
		
	case IDLN_OP_DCL:
		__IDL_tree_free(IDL_OP_DCL(p).op_type_spec);
		__IDL_tree_free(IDL_OP_DCL(p).ident);
		__IDL_tree_free(IDL_OP_DCL(p).parameter_dcls);
		__IDL_tree_free(IDL_OP_DCL(p).raises_expr);
		__IDL_tree_free(IDL_OP_DCL(p).context_expr);
		free(p);
		break;

	case IDLN_PARAM_DCL:
		__IDL_tree_free(IDL_PARAM_DCL(p).param_type_spec);
		__IDL_tree_free(IDL_PARAM_DCL(p).simple_declarator);
		free(p);
		break;
		
	case IDLN_FORWARD_DCL:
		__IDL_tree_free(IDL_FORWARD_DCL(p).ident);
		free(p);
		break;
		
	case IDLN_TYPE_STRING:
		__IDL_tree_free(IDL_TYPE_STRING(p).positive_int_const);
		free(p);
		break;
		
	case IDLN_TYPE_WIDE_STRING:
		__IDL_tree_free(IDL_TYPE_WIDE_STRING(p).positive_int_const);
		free(p);
		break;
		
	case IDLN_TYPE_FIXED:
		__IDL_tree_free(IDL_TYPE_FIXED(p).positive_int_const);
		__IDL_tree_free(IDL_TYPE_FIXED(p).integer_lit);
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

	case IDLN_CASE_STMT:
		__IDL_tree_free(IDL_CASE_STMT(p).labels);
		__IDL_tree_free(IDL_CASE_STMT(p).element_spec);
		free(p);
		break;
		
	case IDLN_INTERFACE:
		__IDL_tree_free(IDL_INTERFACE(p).ident);
		__IDL_tree_free(IDL_INTERFACE(p).inheritance_spec);
		__IDL_tree_free(IDL_INTERFACE(p).body);
		free(p);
		break;

	case IDLN_MODULE:
		__IDL_tree_free(IDL_MODULE(p).ident);
		__IDL_tree_free(IDL_MODULE(p).definition_list);
		free(p);
		break;

	case IDLN_BINOP:
		__IDL_tree_free(IDL_BINOP(p).left);
		__IDL_tree_free(IDL_BINOP(p).right);
		free(p);
		break;

	case IDLN_UNARYOP:
		__IDL_tree_free(IDL_UNARYOP(p).operand);
		free(p);
		break;		
		
	default:
		fprintf(stderr, "warning: free unknown node: %d\n", IDL_NODE_TYPE(p));
		break;
	}
}

void IDL_tree_free(IDL_tree root)
{
	assert(root != NULL);
	__IDL_tree_free(root);
}

IDL_ns IDL_ns_new(void)
{
	IDL_ns ns;

	ns = (IDL_ns)malloc(sizeof(struct _IDL_ns));
	if (ns == NULL) {
		yyerror("IDL_ns_new: memory exhausted");
		return NULL;
	}
	memset(ns, 0, sizeof(struct _IDL_ns));

	IDL_NS(ns).global = IDL_gentree_new(NULL);
	IDL_NS(ns).file = 
		IDL_NS(ns).current = IDL_NS(ns).global;

	return ns;
}

void IDL_ns_free(IDL_ns ns)
{
	assert(ns != NULL);

	__IDL_tree_free(IDL_NS(ns).global);
	
	free(ns);
}

#define IDL_NS_ASSERTS		do {					\
	assert(ns != NULL);						\
	assert(IDL_NS(ns).global != NULL);				\
	assert(IDL_NS(ns).file != NULL);				\
	assert(IDL_NS(ns).current != NULL);				\
	assert(IDL_NODE_TYPE(IDL_NS(ns).global) == IDLN_GENTREE);	\
	assert(IDL_NODE_TYPE(IDL_NS(ns).file) == IDLN_GENTREE);		\
	assert(IDL_NODE_TYPE(IDL_NS(ns).current) == IDLN_GENTREE);	\
} while (0)

static int my_strcmp(const char *a, const char *b)
{
	int rv = strcasecmp(a, b);
	
	if (idl_is_parsing && rv == 0 && strcmp(a, b) != 0)
		yywarning("identifiers should remain same-case after first declaration");

	return rv;
}

static int identcmp(IDL_tree a, IDL_tree b)
{
	assert(IDL_NODE_TYPE(a) == IDLN_IDENT);
	assert(IDL_NODE_TYPE(b) == IDLN_IDENT);
	return my_strcmp(IDL_IDENT(a).str, IDL_IDENT(b).str);
}

int IDL_ns_prefix(IDL_ns ns, const char *s)
{
	IDL_tree p;
	char *r;
	int l;

	IDL_NS_ASSERTS;

	if (s == NULL)
		return IDL_FALSE;
	
	if (*s == '"')
		r = strdup(s + 1);
	else
		r = strdup(s);

	l = strlen(r);
	if (r[l - 1] == '"')
		r[l - 1] = 0;

	p = IDL_gentree_new(IDL_ident_new(r));
	IDL_GENTREE(p).children = IDL_NS(ns).global;
	IDL_NODE_UP(IDL_NS(ns).global) = p;
	IDL_NS(ns).global = p;

	return IDL_TRUE;
}

IDL_tree IDL_ns_resolve_ident(IDL_ns ns, IDL_tree ident)
{
	IDL_tree p, q;
	
	IDL_NS_ASSERTS;

	p = IDL_NS(ns).current;

	while (p != NULL) {

		q = IDL_ns_lookup_this_scope(ns, p, ident);
		
		if (q != NULL)
			return q;

		p = IDL_NODE_UP(p);
	}

	return p;
}

IDL_tree IDL_ns_lookup_this_scope(IDL_ns ns, IDL_tree scope, IDL_tree ident)
{
	IDL_tree p;
	
	IDL_NS_ASSERTS;

	if (scope == NULL)
		return NULL;
	
	assert(IDL_NODE_TYPE(scope) == IDLN_GENTREE);

	p = IDL_GENTREE(scope).children;
	
	for (; p != NULL; p = IDL_GENTREE(p).siblings) {
		if (IDL_GENTREE(p).data == NULL)
			continue;
		assert(IDL_NODE_TYPE(IDL_GENTREE(p).data) == IDLN_IDENT);
		if (identcmp(IDL_GENTREE(p).data, ident) == 0)
			return p;
	}

	return NULL;
}

IDL_tree IDL_ns_lookup_cur_scope(IDL_ns ns, IDL_tree ident)
{
	return IDL_ns_lookup_this_scope(ns, IDL_NS(ns).current, ident);
}

IDL_tree IDL_ns_place_new(IDL_ns ns, IDL_tree ident)
{
	IDL_tree p, up_save;

	IDL_NS_ASSERTS;

	if (IDL_ns_lookup_cur_scope(ns, ident) != NULL)
		return NULL;

	/* don't want to change the ident's parent, since this is in
	   the namespace domain... */
	up_save = IDL_NODE_UP(ident);
	p = IDL_gentree_chain_child(IDL_NS(ns).current, ident);
	IDL_NODE_UP(ident) = up_save;

	if (p == NULL)
		return NULL;

	assert(IDL_NODE_TYPE(p) == IDLN_GENTREE);

	IDL_IDENT_TO_NS(ident) = p;

	assert(IDL_NODE_UP(IDL_IDENT_TO_NS(ident)) == IDL_NS(ns).current);

	return p;
}

void IDL_ns_push_scope(IDL_ns ns, IDL_tree ns_ident)
{
	IDL_NS_ASSERTS;

	assert(IDL_NODE_TYPE(ns_ident) == IDLN_GENTREE);
	assert(IDL_NODE_TYPE(IDL_GENTREE(ns_ident).data) == IDLN_IDENT);
	assert(IDL_NS(ns).current == IDL_NODE_UP(ns_ident));

	IDL_NS(ns).current = ns_ident;
}

void IDL_ns_pop_scope(IDL_ns ns)
{
	IDL_NS_ASSERTS;

	if (IDL_NODE_UP(IDL_NS(ns).current) != NULL)
		IDL_NS(ns).current = IDL_NODE_UP(IDL_NS(ns).current);
}

IDL_tree IDL_ns_qualified_ident_new(IDL_tree nsid)
{
	IDL_tree l = NULL, prev = NULL, tail = NULL;

	while (nsid != NULL) {
		if (IDL_GENTREE(nsid).data == NULL) {
			nsid = IDL_NODE_UP(nsid);
			continue;
		}
		l = IDL_list_new(IDL_ident_new(strdup(IDL_IDENT(IDL_GENTREE(nsid).data).str)));
		if (tail == NULL)
			tail = l;
		IDL_LIST(l).next = prev;
		IDL_LIST(l)._tail = tail;
		prev = l;
		nsid = IDL_NODE_UP(nsid);
	}

	return l;
}

char *IDL_ns_ident_to_qstring(IDL_tree ns_ident, const char *join, int levels)
{
	IDL_tree l, q;
	int len, joinlen;
	char *s;
	int count = 0, start_level;

	if (levels < 0 || levels > 64)
		return NULL;

	if (ns_ident == NULL)
		return NULL;

	assert(IDL_NODE_TYPE(ns_ident) == IDLN_GENTREE);

	l = IDL_ns_qualified_ident_new(ns_ident);

	if (l == NULL)
		return NULL;

	if (join == NULL)
		join = "";

	joinlen = strlen(join);
	for (len = 0, q = l; q != NULL; q = IDL_LIST(q).next) {
		IDL_tree i = IDL_LIST(q).data;
		assert(IDL_NODE_TYPE(q) == IDLN_LIST);
		assert(IDL_NODE_TYPE(i) == IDLN_IDENT);
		if (IDL_IDENT(i).str != NULL)
			len += strlen(IDL_IDENT(i).str) + joinlen;
		++count;
	}

	if (levels == 0)
		start_level = 0;
	else
		start_level = count - levels;

	assert(start_level >= 0 && start_level < count);

	s = (char *)malloc(len + 1);
	
	if (s == NULL) {
		IDL_tree_free(l);
		return NULL;
	}

	s[0] = '\0';

	for (q = l; q != NULL; q = IDL_LIST(q).next) {
		IDL_tree i = IDL_LIST(q).data;
		if (start_level > 0) {
			--start_level;
			continue;
		}
		if (s[0] != '\0')
			strcat(s, join);
		strcat(s, IDL_IDENT(i).str);
	}

	IDL_tree_free(l);

	return s;
}

IDL_tree IDL_get_parent_node(IDL_tree p, IDL_tree_type type, int *levels)
{
	int count = 0;

	if (p == NULL)
		return NULL;

	if (type == IDLN_ANY)
		return IDL_NODE_UP(p);

	while (p != NULL && IDL_NODE_TYPE(p) != type) {

		if (IDL_NODE_IS_SCOPED(p))
			++count;
		
		p = IDL_NODE_UP(p);
	}

	if (p != NULL)
		if (levels != NULL)
			*levels = count;

	return p;
}

int IDL_parse_filename(const char *filename, const char *cpp_args,
		       IDL_callback cb, IDL_tree *tree, IDL_ns *ns,
		       unsigned long parse_flags)
{
	extern void __IDL_lex_init(void);
	extern void __IDL_lex_cleanup(void);
	int yyparse(void);
	extern FILE *__IDL_in;
	FILE *input;
	char *fmt = CPP_PROGRAM " %s %s";
	char *cmd;
	int rv;

	if (!filename ||
	    !tree ||
	    (tree == NULL && ns != NULL)) {
		errno = EINVAL;
		return -1;
	}

	cmd = (char *)malloc(strlen(filename) + 
			     (cpp_args ? strlen(cpp_args) : 0) +
			     strlen(fmt) - 4 + 1);
	if (!cmd) {
		errno = ENOMEM;
		return -1;
	}

	sprintf(cmd, fmt, cpp_args ? cpp_args : "", filename);
	input = popen(cmd, "r");
	free(cmd);

	if (input == NULL || ferror(input))
		return IDL_ERROR;

	__IDL_in = input;
	idl_msgcb = cb;
	flags = parse_flags;
	idl_ns = IDL_ns_new();
	idl_is_parsing = IDL_TRUE;
	__IDL_lex_init();
	__IDL_cur_filename = strdup(filename);
	rv = yyparse();
	idl_is_parsing = IDL_FALSE;
	__IDL_lex_cleanup();
	idl_msgcb = NULL;
	pclose(input);

	if (rv != 0) {
		if (tree)
			*tree = NULL;

		if (ns)
			*ns = NULL;

		return IDL_ERROR;
	}

	if (flags & IDLF_PREFIX_FILENAME)
		IDL_ns_prefix(idl_ns, filename);

	if (tree)
		*tree = idl_root;
	else
		IDL_tree_free(idl_root);

	if (ns)
		*ns = idl_ns;
	else
		IDL_ns_free(idl_ns);

	return IDL_SUCCESS;
}

/* note IDLN_GENTREE is deliberately avoided in assign_up_node, since
   the gentree is primarily for the namespaces... */
static void assign_up_node(IDL_tree up, IDL_tree node)
{
	if (node == NULL)
		return;

	assert(node != up);

	switch (IDL_NODE_TYPE(node)) {
	case IDLN_LIST:
		for (; node != NULL;
		     node = IDL_LIST(node).next)
			if (IDL_NODE_UP(node) == NULL)
				IDL_NODE_UP(node) = up;
		break;

	default:
		if (IDL_NODE_UP(node) == NULL)
			IDL_NODE_UP(node) = up;
		break;
	}
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

IDL_tree IDL_gentree_chain_sibling(IDL_tree from, IDL_tree data)
{
	IDL_tree p;

	if (from == NULL)
		return NULL;

	p = IDL_gentree_new(data);
	IDL_NODE_UP(p) = IDL_NODE_UP(from);

	if (IDL_NODE_UP(from) != NULL)
		from = IDL_GENTREE(IDL_NODE_UP(from)).children;

	assert(from != NULL);

	if (IDL_GENTREE(from).siblings == NULL) {
		IDL_GENTREE(from).siblings =
			IDL_GENTREE(from)._siblings_tail = p;
	} else {
		IDL_GENTREE(IDL_GENTREE(from)._siblings_tail).siblings = p;
		IDL_GENTREE(from)._siblings_tail = p;
	}

	return p;
}

IDL_tree IDL_gentree_chain_child(IDL_tree from, IDL_tree data)
{
	IDL_tree p;

	if (from == NULL)
		return NULL;

	if (IDL_GENTREE(from).children == NULL) {
		p = IDL_gentree_new(data);
		IDL_NODE_UP(p) = from;
		IDL_GENTREE(from).children = p;
	} else {
		p = IDL_gentree_chain_sibling(IDL_GENTREE(from).children, data);
	}

	return p;
}

static IDL_tree IDL_node_new(IDL_tree_type type)
{
	IDL_tree p;

	p = (IDL_tree)malloc(sizeof(IDL_tree_node));
	if (p == NULL) {
		yyerror("IDL_node_new: memory exhausted");
		return NULL;
	}
	memset(p, 0, sizeof(IDL_tree_node));

	IDL_NODE_TYPE(p) = type;

	return p;
}

IDL_tree IDL_list_new(IDL_tree data)
{
	IDL_tree p = IDL_node_new(IDLN_LIST);
	
	assign_up_node(p, data);
	IDL_LIST(p).data = data;

	return p;
}

IDL_tree IDL_gentree_new(IDL_tree data)
{
	IDL_tree p = IDL_node_new(IDLN_GENTREE);
	
	assign_up_node(p, data);
	IDL_GENTREE(p).data = data;
	
	return p;
}

IDL_tree IDL_integer_new(IDL_long_t value)
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

IDL_tree IDL_fixed_new(char *value)
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

IDL_tree IDL_ident_new(char *str)
{
	IDL_tree p = IDL_node_new(IDLN_IDENT);
	
	IDL_IDENT(p).str = str;
	IDL_IDENT(p)._refs = 1;
	
	return p;
}

IDL_tree IDL_member_new(IDL_tree type_spec, IDL_tree dcls)
{
	IDL_tree p = IDL_node_new(IDLN_MEMBER);

	assign_up_node(p, type_spec);
	assign_up_node(p, dcls);
	IDL_MEMBER(p).type_spec = type_spec;
	IDL_MEMBER(p).dcls = dcls;
	
	return p;
}

IDL_tree IDL_type_dcl_new(IDL_tree type_spec, IDL_tree dcls)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_DCL);

	assign_up_node(p, type_spec);
	assign_up_node(p, dcls);
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
	
	assign_up_node(p, positive_int_const);
	assign_up_node(p, integer_lit);
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

	assign_up_node(p, positive_int_const);
	IDL_TYPE_STRING(p).positive_int_const = positive_int_const;

	return p;
}

IDL_tree IDL_type_wide_string_new(IDL_tree positive_int_const)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_WIDE_STRING);
	
	assign_up_node(p, positive_int_const);
	IDL_TYPE_WIDE_STRING(p).positive_int_const = positive_int_const;

	return p;
}

IDL_tree IDL_type_array_new(IDL_tree ident,
			    IDL_tree size_list)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_ARRAY);
	
	assign_up_node(p, ident);
	assign_up_node(p, size_list);
	IDL_TYPE_ARRAY(p).ident = ident;
	IDL_TYPE_ARRAY(p).size_list = size_list;

	return p;
}

IDL_tree IDL_type_sequence_new(IDL_tree simple_type_spec,
			       IDL_tree positive_int_const)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_SEQUENCE);

	assign_up_node(p, simple_type_spec);
	assign_up_node(p, positive_int_const);
	IDL_TYPE_SEQUENCE(p).simple_type_spec = simple_type_spec;
	IDL_TYPE_SEQUENCE(p).positive_int_const = positive_int_const;

	return p;
}

IDL_tree IDL_type_struct_new(IDL_tree ident, IDL_tree member_list)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_STRUCT);
	
	assign_up_node(p, ident);
	assign_up_node(p, member_list);
	IDL_TYPE_STRUCT(p).ident = ident;
	IDL_TYPE_STRUCT(p).member_list = member_list;

	return p;
}

IDL_tree IDL_type_union_new(IDL_tree ident, IDL_tree switch_type_spec, IDL_tree switch_body)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_UNION);

	assign_up_node(p, ident);
	assign_up_node(p, switch_type_spec);
	assign_up_node(p, switch_body);
	IDL_TYPE_UNION(p).ident = ident;
	IDL_TYPE_UNION(p).switch_type_spec = switch_type_spec;
	IDL_TYPE_UNION(p).switch_body = switch_body;

	return p;
}

IDL_tree IDL_type_enum_new(IDL_tree ident, IDL_tree enumerator_list)
{
	IDL_tree p = IDL_node_new(IDLN_TYPE_ENUM);
	
	assign_up_node(p, ident);
	assign_up_node(p, enumerator_list);
	IDL_TYPE_ENUM(p).ident = ident;
	IDL_TYPE_ENUM(p).enumerator_list = enumerator_list;

	return p;
}

IDL_tree IDL_case_stmt_new(IDL_tree labels, IDL_tree element_spec)
{
	IDL_tree p = IDL_node_new(IDLN_CASE_STMT);
	
	assign_up_node(p, labels);
	assign_up_node(p, element_spec);
	IDL_CASE_STMT(p).labels = labels;
	IDL_CASE_STMT(p).element_spec = element_spec;

	return p;
}

IDL_tree IDL_interface_new(IDL_tree ident, IDL_tree inheritance_spec, IDL_tree body)
{
	IDL_tree p = IDL_node_new(IDLN_INTERFACE);

	assign_up_node(p, ident);
	assign_up_node(p, inheritance_spec);
	assign_up_node(p, body);
	IDL_INTERFACE(p).ident = ident;
	IDL_INTERFACE(p).inheritance_spec = inheritance_spec;
	IDL_INTERFACE(p).body = body;

	return p;
}

IDL_tree IDL_module_new(IDL_tree ident, IDL_tree definition_list)
{
	IDL_tree p = IDL_node_new(IDLN_MODULE);
	
	assign_up_node(p, ident);
	assign_up_node(p, definition_list);
	IDL_MODULE(p).ident = ident;
	IDL_MODULE(p).definition_list = definition_list;

	return p;
}

IDL_tree IDL_binop_new(enum IDL_binop op, IDL_tree left, IDL_tree right)
{
	IDL_tree p = IDL_node_new(IDLN_BINOP);
	
	assign_up_node(p, left);
	assign_up_node(p, right);
	IDL_BINOP(p).op = op;
	IDL_BINOP(p).left = left;
	IDL_BINOP(p).right = right;

	return p;
}

IDL_tree IDL_unaryop_new(enum IDL_unaryop op, IDL_tree operand)
{
	IDL_tree p = IDL_node_new(IDLN_UNARYOP);
	
	assign_up_node(p, operand);
	IDL_UNARYOP(p).op = op;
	IDL_UNARYOP(p).operand = operand;

	return p;
}

IDL_tree IDL_const_dcl_new(IDL_tree const_type, IDL_tree ident, IDL_tree const_exp)
{
	IDL_tree p = IDL_node_new(IDLN_CONST_DCL);
	
	assign_up_node(p, const_type);
	assign_up_node(p, ident);
	assign_up_node(p, const_exp);
	IDL_CONST_DCL(p).const_type = const_type;
	IDL_CONST_DCL(p).ident = ident;
	IDL_CONST_DCL(p).const_exp = const_exp;

	return p;
}

IDL_tree IDL_except_dcl_new(IDL_tree ident, IDL_tree members)
{
	IDL_tree p = IDL_node_new(IDLN_EXCEPT_DCL);
	
	assign_up_node(p, ident);
	assign_up_node(p, members);
	IDL_EXCEPT_DCL(p).ident = ident;
	IDL_EXCEPT_DCL(p).members = members;

	return p;
}

IDL_tree IDL_attr_dcl_new(unsigned f_readonly,
			  IDL_tree param_type_spec,
			  IDL_tree simple_declarations)
{
	IDL_tree p = IDL_node_new(IDLN_ATTR_DCL);
	
	assign_up_node(p, param_type_spec);
	assign_up_node(p, simple_declarations);
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
	
	assign_up_node(p, op_type_spec);
	assign_up_node(p, ident);
	assign_up_node(p, parameter_dcls);
	assign_up_node(p, raises_expr);
	assign_up_node(p, context_expr);
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
	
	assign_up_node(p, param_type_spec);
	assign_up_node(p, simple_declarator);
	IDL_PARAM_DCL(p).attr = attr;
	IDL_PARAM_DCL(p).param_type_spec = param_type_spec;
	IDL_PARAM_DCL(p).simple_declarator = simple_declarator;

	return p;
}

IDL_tree IDL_forward_dcl_new(IDL_tree ident)
{
	IDL_tree p = IDL_node_new(IDLN_FORWARD_DCL);

	/* forward declarations should let a later interface
	   declaration assign the up node */
	IDL_FORWARD_DCL(p).ident = ident;

	return p;
}

static int IDL_binop_chktypes(enum IDL_binop op, IDL_tree a, IDL_tree b)
{
	if (IDL_NODE_TYPE(a) != IDLN_BINOP &&
	    IDL_NODE_TYPE(b) != IDLN_BINOP &&
	    IDL_NODE_TYPE(a) != IDLN_UNARYOP &&
	    IDL_NODE_TYPE(b) != IDLN_UNARYOP &&
	    IDL_NODE_TYPE(a) != IDL_NODE_TYPE(b)) {
		yyerror("invalid mix of types in constant expression");
		return -1;
	}

	switch (op) {
	case IDL_BINOP_MULT:
	case IDL_BINOP_DIV:
	case IDL_BINOP_ADD:
	case IDL_BINOP_SUB:
		break;

	case IDL_BINOP_MOD:
	case IDL_BINOP_SHR:
	case IDL_BINOP_SHL:
	case IDL_BINOP_AND:
	case IDL_BINOP_OR:
	case IDL_BINOP_XOR:
		if ((IDL_NODE_TYPE(a) != IDLN_INTEGER ||
		     IDL_NODE_TYPE(b) != IDLN_INTEGER) &&
		    !(IDL_NODE_TYPE(a) == IDLN_BINOP ||
		      IDL_NODE_TYPE(b) == IDLN_BINOP ||
		      IDL_NODE_TYPE(a) == IDLN_UNARYOP ||
		      IDL_NODE_TYPE(b) == IDLN_UNARYOP)) {
			yyerror("invalid operation on non-integer value");
			return -1;
		}
		break;
	}

	return 0;
}

static int IDL_unaryop_chktypes(enum IDL_unaryop op, IDL_tree a)
{
	switch (op) {
	case IDL_UNARYOP_PLUS:
	case IDL_UNARYOP_MINUS:
		break;

	case IDL_UNARYOP_COMPLEMENT:
		if (IDL_NODE_TYPE(a) != IDLN_INTEGER &&
		    !(IDL_NODE_TYPE(a) == IDLN_BINOP ||
		      IDL_NODE_TYPE(a) == IDLN_UNARYOP)) {
			yyerror("operand to complement must be integer");
			return -1;
		}
		break;
	}

	return 0;
}

static IDL_tree IDL_binop_eval_integer(enum IDL_binop op, IDL_tree a, IDL_tree b)
{
	IDL_tree p = NULL;

	assert(IDL_NODE_TYPE(a) == IDLN_INTEGER);

	switch (op) {
	case IDL_BINOP_MULT:
		p = IDL_integer_new(IDL_INTEGER(a).value * IDL_INTEGER(b).value);
		break;

	case IDL_BINOP_DIV:
		if (IDL_INTEGER(b).value == 0) {
			yyerror("divide by zero in constant expression");
			return NULL;
		}
		p = IDL_integer_new(IDL_INTEGER(a).value / IDL_INTEGER(b).value);
		break;

	case IDL_BINOP_ADD:
		p = IDL_integer_new(IDL_INTEGER(a).value + IDL_INTEGER(b).value);
		break;

	case IDL_BINOP_SUB:
		p = IDL_integer_new(IDL_INTEGER(a).value - IDL_INTEGER(b).value);
		break;

	case IDL_BINOP_MOD:
		if (IDL_INTEGER(b).value == 0) {
			yyerror("modulo by zero in constant expression");
			return NULL;
		}
		p = IDL_integer_new(IDL_INTEGER(a).value % IDL_INTEGER(b).value);
		break;

	case IDL_BINOP_SHR:
		p = IDL_integer_new(IDL_INTEGER(a).value >> IDL_INTEGER(b).value);
		break;

	case IDL_BINOP_SHL:
		p = IDL_integer_new(IDL_INTEGER(a).value << IDL_INTEGER(b).value);
		break;

	case IDL_BINOP_AND:
		p = IDL_integer_new(IDL_INTEGER(a).value & IDL_INTEGER(b).value);
		break;

	case IDL_BINOP_OR:
		p = IDL_integer_new(IDL_INTEGER(a).value | IDL_INTEGER(b).value);
		break;

	case IDL_BINOP_XOR:
		p = IDL_integer_new(IDL_INTEGER(a).value ^ IDL_INTEGER(b).value);
		break;
	}

	return p;
}

#if 0
static IDL_tree IDL_binop_eval_fixed(enum IDL_binop op, IDL_tree a, IDL_tree b)
{
	IDL_tree p = NULL;

	assert(IDL_NODE_TYPE(a) == IDLN_FIXED);

	switch (op) {
	case IDL_BINOP_MULT:
		p = IDL_fixed_new(IDL_FIXED(a).value * IDL_FIXED(b).value);
		break;

	case IDL_BINOP_DIV:
		if (IDL_FIXED(b).value == 0.0) {
			yyerror("divide by zero in constant expression");
			return NULL;
		}
		p = IDL_fixed_new(IDL_FIXED(a).value / IDL_FIXED(b).value);
		break;

	case IDL_BINOP_ADD:
		p = IDL_fixed_new(IDL_FIXED(a).value + IDL_FIXED(b).value);
		break;

	case IDL_BINOP_SUB:
		p = IDL_fixed_new(IDL_FIXED(a).value - IDL_FIXED(b).value);
		break;

	default:
		break;
	}

	return p;
}
#endif

static IDL_tree IDL_binop_eval_float(enum IDL_binop op, IDL_tree a, IDL_tree b)
{
	IDL_tree p = NULL;

	assert(IDL_NODE_TYPE(a) == IDLN_FLOAT);

	switch (op) {
	case IDL_BINOP_MULT:
		p = IDL_float_new(IDL_FLOAT(a).value * IDL_FLOAT(b).value);
		break;

	case IDL_BINOP_DIV:
		if (IDL_FLOAT(b).value == 0.0) {
			yyerror("divide by zero in constant expression");
			return NULL;
		}
		p = IDL_float_new(IDL_FLOAT(a).value / IDL_FLOAT(b).value);
		break;

	case IDL_BINOP_ADD:
		p = IDL_float_new(IDL_FLOAT(a).value + IDL_FLOAT(b).value);
		break;

	case IDL_BINOP_SUB:
		p = IDL_float_new(IDL_FLOAT(a).value - IDL_FLOAT(b).value);
		break;

	default:
		break;
	}

	return p;
}

static IDL_tree IDL_binop_eval(enum IDL_binop op, IDL_tree a, IDL_tree b)
{
	assert(IDL_NODE_TYPE(a) == IDL_NODE_TYPE(b));

	switch (IDL_NODE_TYPE(a)) {
	case IDLN_INTEGER: return IDL_binop_eval_integer(op, a, b);
#if 0
	case IDLN_FIXED: return IDL_binop_eval_fixed(op, a, b);
#endif
	case IDLN_FLOAT: return IDL_binop_eval_float(op, a, b);
	default: return NULL;
	}
}

static IDL_tree IDL_unaryop_eval_integer(enum IDL_unaryop op, IDL_tree a)
{
	IDL_tree p = NULL;

	assert(IDL_NODE_TYPE(a) == IDLN_INTEGER);

	switch (op) {
	case IDL_UNARYOP_PLUS:
		p = IDL_integer_new(IDL_INTEGER(a).value);
		break;

	case IDL_UNARYOP_MINUS:
		p = IDL_integer_new(-IDL_INTEGER(a).value);
		break;

	case IDL_UNARYOP_COMPLEMENT:
		p = IDL_integer_new(~IDL_INTEGER(a).value);
		break;
	}
       
	return p;
}

static IDL_tree IDL_unaryop_eval_fixed(enum IDL_unaryop op, IDL_tree a)
{
	IDL_tree p = NULL;

	assert(IDL_NODE_TYPE(a) == IDLN_FIXED);

	switch (op) {
	case IDL_UNARYOP_PLUS:
		p = IDL_fixed_new(IDL_FIXED(a).value);
		break;
#if 0
	case IDL_UNARYOP_MINUS:
		p = IDL_fixed_new(-IDL_FIXED(a).value);
		break;
#endif

	default:
		break;
	}
       
	return p;
}

static IDL_tree IDL_unaryop_eval_float(enum IDL_unaryop op, IDL_tree a)
{
	IDL_tree p = NULL;

	assert(IDL_NODE_TYPE(a) == IDLN_FLOAT);

	switch (op) {
	case IDL_UNARYOP_PLUS:
		p = IDL_float_new(IDL_FLOAT(a).value);
		break;

	case IDL_UNARYOP_MINUS:
		p = IDL_float_new(-IDL_FLOAT(a).value);
		break;

	default:
		break;
	}
       
	return p;
}

static IDL_tree IDL_unaryop_eval(enum IDL_unaryop op, IDL_tree a)
{
	switch (IDL_NODE_TYPE(a)) {
	case IDLN_INTEGER: return IDL_unaryop_eval_integer(op, a);
	case IDLN_FIXED: return IDL_unaryop_eval_fixed(op, a);
	case IDLN_FLOAT: return IDL_unaryop_eval_float(op, a);
	default: return NULL;
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
