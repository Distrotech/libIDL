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
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
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
static int			IDL_ns_check_for_ambiguous_inheritance(IDL_tree interface_ident,
								       IDL_tree p);
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
static int			do_token_error(IDL_tree p, const char *message, int prev);
static int			get_node_info(IDL_tree p, char **who, char **what);

#ifndef HAVE_CPP_PIPE_STDIN
char *				__IDL_tmp_filename = NULL;
#endif
const char *			__IDL_real_filename = NULL;
char *				__IDL_cur_filename = NULL;
int				__IDL_cur_line;
static unsigned long		flags;
static int			idl_nerrors, idl_nwarnings;
static IDL_tree			idl_root;
static IDL_ns			idl_ns;
static IDL_callback		idl_msgcb;
static int			idl_is_okay;
static int			idl_is_parsing;
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
%type <tree>			const_type new_ident illegal_ident
/*
%type <tree>			prev_ident new_or_prev_ident global_ident ns_new_or_prev_ident 
*/
%type <tree>			ident floating_pt_type integer_type
%type <tree>			char_type wide_char_type boolean_type octet_type
%type <tree>			string_type wide_string_type fixed_pt_type fixed_pt_const_type
%type <tree>			any_type object_type enum_type scoped_name
%type <tree>			case_stmt case_label case_label_list fixed_array_size_list
%type <tree>			case_stmt_list fixed_array_size positive_int_const
%type <tree>			ns_scoped_name ns_prev_ident ns_new_ident ns_global_ident
%type <tree>			cur_ns_new_or_prev_ident useless_semicolon
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

specification:		/* empty */			{ yyerror("Empty file"); YYABORT; }
|			definition_list
	;

definition_list:	definition			{ $$ = list_start($1); }
|			definition_list definition	{ $$ = list_chain($1, $2); }
	;

check_semicolon:	';'
|			/* empty */			{
	if (do_token_error($<tree>0, "Missing semicolon after", 1))
		YYABORT;
}
	;

useless_semicolon:	';'				{
	yyerror("Dangling semicolon has no effect");
	$$ = NULL;
}
	;

check_comma:		','
|			/* empty */			{
	if (do_token_error($<tree>0, "Missing comma after", 1))
		YYABORT;
}
	;

illegal_ident:		scoped_name			{
	assert(IDL_NODE_UP($1) != NULL);
	do_token_error(IDL_NODE_UP($1), "Illegal context for", 0);
}
	;

definition:		type_dcl check_semicolon
|			const_dcl check_semicolon
|			except_dcl check_semicolon
|			interface check_semicolon
|			module check_semicolon
|			illegal_ident
|			useless_semicolon
	;

interface:		interface_dcl
|			forward_dcl
	;

module:			TOK_MODULE new_or_prev_scope '{'
				definition_list
			'}' pop_scope			{
	if (IDL_NODE_UP($2) != NULL &&
	    IDL_NODE_TYPE(IDL_NODE_UP($2)) != IDLN_MODULE) {
		do_token_error(IDL_NODE_UP($2), "Module definition conflicts with", 0);
		YYABORT;
	}
	$$ = IDL_module_new($2, $4);
}
|			TOK_MODULE new_or_prev_scope '{'
			'}' pop_scope			{
	if (IDL_NODE_UP($2) != NULL &&
	    IDL_NODE_TYPE(IDL_NODE_UP($2)) != IDLN_MODULE) {
		do_token_error(IDL_NODE_UP($2), "Module definition conflicts with", 0);
		YYABORT;
	}
	yyerrorv("Empty module declaration `%s' is not legal IDL", IDL_IDENT($2).str);
	$$ = IDL_module_new($2, NULL);
}
	;

interface_dcl:		TOK_INTERFACE new_or_prev_scope
			pop_scope
			z_inheritance			{ 
	assert($2 != NULL);
	assert(IDL_NODE_TYPE($2) == IDLN_IDENT);
	assert(IDL_IDENT_TO_NS($2) != NULL);
	assert(IDL_NODE_TYPE(IDL_IDENT_TO_NS($2)) == IDLN_GENTREE);
	if (IDL_NODE_UP($2) != NULL &&
	    IDL_NODE_TYPE(IDL_NODE_UP($2)) != IDLN_INTERFACE &&
	    IDL_NODE_TYPE(IDL_NODE_UP($2)) != IDLN_FORWARD_DCL) {
		do_token_error(IDL_NODE_UP($2), "Interface definition conflicts with", 0);
		YYABORT;
	} else if (IDL_NODE_UP($2) != NULL &&
		   IDL_NODE_TYPE(IDL_NODE_UP($2)) != IDLN_FORWARD_DCL) {
		yyerrorv("Cannot redeclare interface `%s'", IDL_IDENT($2).str);
		YYABORT;
	}
	IDL_GENTREE(IDL_IDENT_TO_NS($2))._import = $4;
	IDL_ns_push_scope(idl_ns, IDL_IDENT_TO_NS($2));
	if (IDL_ns_check_for_ambiguous_inheritance($2, $4))
		idl_is_okay = IDL_FALSE;
}
			'{'
				interface_body
			'}' pop_scope			{ $$ = IDL_interface_new($2, $4, $7); }
	;

forward_dcl:		TOK_INTERFACE
			new_or_prev_scope pop_scope	{ $$ = IDL_forward_dcl_new($2); }
	;

z_inheritance:		/* empty */			{ $$ = NULL; }
|			':' scoped_name_list		{
	IDL_tree p = $2;

	assert(IDL_NODE_TYPE(p) == IDLN_LIST);
	for (; p != NULL; p = IDL_LIST(p).next) {
		assert(IDL_LIST(p).data != NULL);
		assert(IDL_NODE_TYPE(IDL_LIST(p).data) == IDLN_IDENT);
		if (IDL_NODE_TYPE(IDL_NODE_UP(IDL_LIST(p).data)) != IDLN_INTERFACE) {
			yyerrorv("`%s' is not an interface",
				 IDL_IDENT(IDL_LIST(p).data).str);
			YYABORT;
		}
	}
	$$ = $2;
}
	;

scoped_name_list:	scoped_name			{ $$ = list_start($1); }
|			scoped_name_list
			check_comma scoped_name		{ $$ = list_chain($1, $3); }
	;

interface_body:		export_list
	;

export_list:		/* empty */			{ $$ = NULL; }
|			export_list export		{ $$ = zlist_chain($1, $2); }
	;

export:			type_dcl check_semicolon
|			except_dcl check_semicolon
|			op_dcl check_semicolon
|			attr_dcl check_semicolon
|			const_dcl check_semicolon
|			useless_semicolon
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
			element_spec check_semicolon	{ $$ = IDL_case_stmt_new($1, $2); }
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
|			param_dcl_list
			check_comma param_dcl		{ $$ = list_chain($1, $3); }
	;

param_dcl:		param_attribute
			param_type_spec
			simple_declarator		{ $$ = IDL_param_dcl_new($1, $2, $3); }
	;

param_attribute:	TOK_IN				{ $$ = IDL_PARAM_IN; }
|			TOK_OUT				{ $$ = IDL_PARAM_OUT; }
|			TOK_INOUT			{ $$ = IDL_PARAM_INOUT; }
|			param_type_spec			{
	yyerrorv("Missing direction attribute (in, out, inout) before parameter");
	IDL_tree_free($1);
}
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
		yyerror("Illegal type in constant expression");
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
|			TOK_OP_SCOPE ns_global_ident	{ $$ = $2; }
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
		yyerrorv("`%s' undeclared identifier", IDL_IDENT($3).str);
		IDL_tree_free($3);
		YYABORT;
	}
	IDL_tree_free($3);
	++IDL_IDENT(IDL_GENTREE(p).data)._refs;
	$$ = p;
}
	;

enumerator_list:	new_ident			{ $$ = list_start($1); }
|			enumerator_list
			check_comma new_ident		{ $$ = list_chain($1, $3); }
	;

member_list:		member				{ $$ = list_start($1); }
|			member_list member		{ $$ = list_chain($1, $2); }
	;

member:			type_spec declarator_list
			check_semicolon			{ $$ = IDL_member_new($1, $2); }
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
			check_comma declarator		{ $$ = list_chain($1, $3); }
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
			check_comma simple_declarator	{ $$ = list_chain($1, $3); }
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

/*

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
*/

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
		p = IDL_ns_lookup_cur_scope(idl_ns, $1);
		if (p && IDL_GENTREE(p).data &&
		    IDL_NODE_UP(IDL_GENTREE(p).data) &&
		    IDL_NODE_UP(IDL_NODE_UP(IDL_GENTREE(p).data)))
			do_token_error(IDL_NODE_UP(IDL_NODE_UP(IDL_GENTREE(p).data)),
				       "Duplicate identifier conflicts with", 0);
		else
			yyerrorv("`%s' duplicate identifier", IDL_IDENT($1).str);
		IDL_tree_free($1);
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
		yyerrorv("`%s' undeclared identifier", IDL_IDENT($1).str);
		IDL_tree_free($1);
		YYABORT;
	}
	IDL_tree_free($1);
	assert(IDL_GENTREE(p).data != NULL);
	assert(IDL_IDENT(IDL_GENTREE(p).data)._ns_ref == p);
	++IDL_IDENT(IDL_GENTREE(p).data)._refs;
	$$ = p;
}
	;

/*

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
*/

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
		yyerrorv("`%s' undeclared identifier", IDL_IDENT($1).str);
		IDL_tree_free($1);
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
|			string_lit_list
			check_comma string_lit		{ $$ = list_chain($1, $3); }
	;

positive_int_const:	TOK_INTEGER			{
	if ($1 < 0) {
		yywarningv("Cannot use negative value " 
			   IDL_SB10_FMT ", using " IDL_SB10_FMT, $1, -$1);
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
	gchar *filename = g_basename(__IDL_cur_filename);

	++idl_nerrors;
	idl_is_okay = IDL_FALSE;
	
	if (idl_msgcb)
		(*idl_msgcb)(IDL_ERROR, idl_nerrors, line, filename, s);
	else
		fprintf(stderr, "%s:%d: Error: %s\n", filename, line, s);
}

void yywarningl(const char *s, int ofs)
{
	int line = __IDL_cur_line - 1 + ofs;
	gchar *filename = g_basename(__IDL_cur_filename);
	
	++idl_nwarnings;
	
	if (idl_msgcb)
		(*idl_msgcb)(IDL_WARNING, idl_nwarnings, line, filename, s);
	else
		fprintf(stderr, "%s:%d: Warning: %s\n", filename, line, s);
}

void yyerror(const char *s)
{
	yyerrorl(s, 0);
}

void yywarning(const char *s)
{
	yywarningl(s, 0);
}

void yyerrorlv(const char *fmt, int ofs, ...)
{
	char *msg = (char *)malloc(strlen(fmt) + 2048);
	va_list args;

	va_start(args, ofs);
	vsprintf(msg, fmt, args);
	yyerrorl(msg, ofs);
	va_end(args);
	free(msg);
}

void yywarninglv(const char *fmt, int ofs, ...)
{
	char *msg = (char *)malloc(strlen(fmt) + 2048);
	va_list args;

	va_start(args, ofs);
	vsprintf(msg, fmt, args);
	yywarningl(msg, ofs);
	va_end(args);
	free(msg);
}

void yyerrorv(const char *fmt, ...)
{
	char *msg = (char *)malloc(strlen(fmt) + 2048);
	va_list args;

	va_start(args, fmt);
	vsprintf(msg, fmt, args);
	yyerror(msg);
	va_end(args);
	free(msg);
}

void yywarningv(const char *fmt, ...)
{
	char *msg = (char *)malloc(strlen(fmt) + 2048);
	va_list args;

	va_start(args, fmt);
	vsprintf(msg, fmt, args);
	yywarning(msg);
	va_end(args);
	free(msg);
}

const char *IDL_get_libver_string(void)
{
	return VERSION;
}

const char *IDL_get_IDLver_string(void)
{
	return "2.2";
}

static const char *IDL_ns_get_cur_prefix(IDL_ns ns)
{
	IDL_tree p;

	p = IDL_NS(ns).current;

	assert(p != NULL);

	while (p && !IDL_GENTREE(p)._cur_prefix)
		p = IDL_NODE_UP(p);

	return p ? IDL_GENTREE(p)._cur_prefix : NULL;
}

char *IDL_ns_ident_make_repo_id(IDL_ns ns, IDL_tree p,
				const char *p_prefix, int *major, int *minor)
{
	GString *s = g_string_new(NULL);
	const char *prefix;
	char *q;

	assert(p != NULL);
	
	if (IDL_NODE_TYPE(p) == IDLN_IDENT)
		p = IDL_IDENT_TO_NS(p);

	assert(p != NULL);

	prefix = p_prefix ? p_prefix : IDL_ns_get_cur_prefix(ns);

	q = IDL_ns_ident_to_qstring(p, "/", 0);
	g_string_sprintf(s, "IDL:%s%s%s:%d.%d",
			 prefix ? prefix : "",
			 prefix && *prefix ? "/" : "",
			 q,
			 major ? *major : 1,
			 minor ? *minor : 0);
	free(q);

	q = s->str;
	g_string_free(s, FALSE);

	return q;
}

static const char *get_name_token(const char *s, char **tok)
{
	const char *begin = s;
	int state = 0;

	if (!s)
		return NULL;

	while (isspace(*s)) ++s;
	
	while (1) switch (state) {
	case 0:		/* Unknown */
		if (*s == ':')
			state = 1;
		else if (isalnum(*s) || *s == '_') {
			begin = s;
			state = 2;
		} else
			return NULL;
		break;
	case 1:		/* Scope */
		if (strncmp(s, "::", 2) == 0) {
			char *r = (char *)malloc(3);
			strcpy(r, "::");
			*tok = r;
			return s + 2;
		} else	/* Invalid */
			return NULL;
		break;
	case 2:
		if (isalnum(*s) || *s == '_')
			++s;
		else {
			char *r = (char *)malloc(s - begin + 1);
			strncpy(r, begin, s - begin + 1);
			r[s - begin] = 0;
			*tok = r;
			return s;
		}
		break;
	}
}

static IDL_tree IDL_ns_pragma_parse_name(IDL_ns ns, const char *s)
{
	IDL_tree p = IDL_NS(ns).current;
	int start = 1;
	char *tok;

	while (p && *s && (s = get_name_token(s, &tok))) {
		if (tok == NULL)
			return NULL;
		if (strcmp(tok, "::") == 0) {
			if (start) {
				/* Globally scoped */
				p = IDL_NS(ns).file;
			}
			free(tok);
		} else {
			IDL_tree ident = IDL_ident_new(tok);
			p = IDL_ns_lookup_this_scope(idl_ns, p, ident);
			IDL_tree_free(ident);
		}
		start = 0;
	}
	
	return p;
}

void IDL_ns_ID(IDL_ns ns, const char *s)
{
	char name[1024], id[1024];
	IDL_tree p, ident;
	int n;

	n = sscanf(s, "%1023s \"%1023s\"", name, id);
	if (n < 2 && idl_is_parsing) {
		yywarning("Malformed pragma ID");
		return;
	}
	if (id[strlen(id) - 1] == '"')
		id[strlen(id) - 1] = 0;

	p = IDL_ns_pragma_parse_name(idl_ns, name);
	if (!p && idl_is_parsing) {
		yywarningv("Unknown identifier `%s' in pragma ID", name);
		return;
	}

	/* We have resolved the identifier, so assign the repo id */
	assert(IDL_NODE_TYPE(p) == IDLN_GENTREE);
	assert(IDL_GENTREE(p).data != NULL);
	assert(IDL_NODE_TYPE(IDL_GENTREE(p).data) == IDLN_IDENT);
	ident = IDL_GENTREE(p).data;

	if (IDL_IDENT_REPO_ID(ident) != NULL)
		free(IDL_IDENT_REPO_ID(ident));

	IDL_IDENT_REPO_ID(ident) = strdup(id);
}

void IDL_ns_version(IDL_ns ns, const char *s)
{
	char name[1024];
	int n, major, minor;
	IDL_tree p, ident;

	n = sscanf(s, "%1023s %u %u", name, &major, &minor);
	if (n < 3 && idl_is_parsing) {
		yywarning("Malformed pragma version");
		return;
	}

	p = IDL_ns_pragma_parse_name(idl_ns, name);
	if (!p && idl_is_parsing) {
		yywarningv("Unknown identifier `%s' in pragma version", name);
		return;
	}

	/* We have resolved the identifier, so assign the repo id */
	assert(IDL_NODE_TYPE(p) == IDLN_GENTREE);
	assert(IDL_GENTREE(p).data != NULL);
	assert(IDL_NODE_TYPE(IDL_GENTREE(p).data) == IDLN_IDENT);
	ident = IDL_GENTREE(p).data;

	if (IDL_IDENT_REPO_ID(ident) != NULL) {
		char *v = strrchr(IDL_IDENT_REPO_ID(ident), ':');
		if (v) {
			GString *s;

			*v = 0;
			s = g_string_new(NULL);
			g_string_sprintf(s, "%s:%d.%d",
					 IDL_IDENT_REPO_ID(ident), major, minor);
			free(IDL_IDENT_REPO_ID(ident));
			IDL_IDENT_REPO_ID(ident) = s->str;
			g_string_free(s, FALSE);
		} else if (idl_is_parsing)
			yywarningv("Cannot find RepositoryID OMG IDL version in ID `%s'",
				   IDL_IDENT_REPO_ID(ident));
	} else
		IDL_IDENT_REPO_ID(ident) = IDL_ns_ident_make_repo_id(idl_ns, p, NULL, &major, &minor);
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
	else if (strcmp(directive, "ID") == 0)
		IDL_ns_ID(idl_ns, s);
	else if (strcmp(directive, "version") == 0)
		IDL_ns_version(idl_ns, s);
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

int IDL_list_length(IDL_tree list)
{
	int retval;
	IDL_tree curitem;

	for(curitem = list, retval = 0; curitem;
	    curitem = IDL_LIST(curitem).next)
		retval++;

	return retval;
}

IDL_tree IDL_list_nth(IDL_tree list, int n)
{
	IDL_tree curitem;
	int i;
	for(curitem = list, i = 0; i < n && curitem;
	    curitem = IDL_LIST(curitem).next, i++)
		/* */;
	return curitem;
}

static int do_token_error(IDL_tree p, const char *message, int prev)
{
	int dienow;
	char *what = NULL, *who = NULL;

	assert(p != NULL);

	dienow = get_node_info(p, &what, &who);

	assert(what != NULL);
	
	if (who && *who)
		yyerrorlv("%s %s `%s'",
			  prev ? __IDL_prev_token_line - __IDL_cur_token_line : 0,
			  message, what, who);
	else
		yyerrorlv("%s %s",
			  prev ? __IDL_prev_token_line - __IDL_cur_token_line : 0,
			  message, what);
	
	return dienow;
}

static int get_node_info(IDL_tree p, char **what, char **who)
{
	int dienow = 0;

	assert(what != NULL);
	assert(who != NULL);

	switch (IDL_NODE_TYPE(p)) {
	case IDLN_TYPE_STRUCT:
		*what = "structure definition";
		*who = IDL_IDENT(IDL_TYPE_STRUCT(p).ident).str;
		break;
	case IDLN_TYPE_UNION:
		*what = "union definition";
		*who = IDL_IDENT(IDL_TYPE_UNION(p).ident).str;
		break;
	case IDLN_TYPE_ENUM:
		*what = "enumeration definition";
		*who = IDL_IDENT(IDL_TYPE_ENUM(p).ident).str;
		break;
	case IDLN_IDENT:
		*what = "identifier";
		*who = IDL_IDENT(p).str;
		break;
	case IDLN_TYPE_DCL:
		*what = "type definition";
		assert(IDL_TYPE_DCL(p).dcls != NULL);
		assert(IDL_NODE_TYPE(IDL_TYPE_DCL(p).dcls) == IDLN_LIST);
		assert(IDL_LIST(IDL_TYPE_DCL(p).dcls)._tail != NULL);
		assert(IDL_NODE_TYPE(IDL_LIST(IDL_TYPE_DCL(p).dcls)._tail) == IDLN_LIST);
		*who = IDL_IDENT(IDL_LIST(IDL_LIST(IDL_TYPE_DCL(p).dcls)._tail).data).str;
		break;
	case IDLN_MEMBER:
		*what = "member declaration";
		assert(IDL_MEMBER(p).dcls != NULL);
		assert(IDL_NODE_TYPE(IDL_MEMBER(p).dcls) == IDLN_LIST);
		assert(IDL_LIST(IDL_MEMBER(p).dcls)._tail != NULL);
		assert(IDL_NODE_TYPE(IDL_LIST(IDL_MEMBER(p).dcls)._tail) == IDLN_LIST);
		*who = IDL_IDENT(IDL_LIST(IDL_LIST(IDL_MEMBER(p).dcls)._tail).data).str;
		break;
	case IDLN_LIST:
		if (!IDL_LIST(p).data)
			break;
		assert(IDL_LIST(p)._tail != NULL);
		if (!IDL_LIST(IDL_LIST(p)._tail).data)
			break;
		dienow = get_node_info(IDL_LIST(IDL_LIST(p)._tail).data, what, who);
		break;
	case IDLN_ATTR_DCL:
		*what = "interface attribute";
		assert(IDL_ATTR_DCL(p).simple_declarations != NULL);
		assert(IDL_NODE_TYPE(IDL_ATTR_DCL(p).simple_declarations) == IDLN_LIST);
		assert(IDL_LIST(IDL_ATTR_DCL(p).simple_declarations)._tail != NULL);
		assert(IDL_NODE_TYPE(IDL_LIST(IDL_ATTR_DCL(p).simple_declarations)._tail) == IDLN_LIST);
		*who = IDL_IDENT(IDL_LIST(IDL_LIST(IDL_ATTR_DCL(p).simple_declarations)._tail).data).str;
		break;
	case IDLN_PARAM_DCL:
		*what = "operation parameter";
		assert(IDL_PARAM_DCL(p).simple_declarator != NULL);
		assert(IDL_NODE_TYPE(IDL_PARAM_DCL(p).simple_declarator) = IDLN_IDENT);
		*who = IDL_IDENT(IDL_PARAM_DCL(p).simple_declarator).str;
		break;
	case IDLN_CONST_DCL:
		*what = "constant declaration for";
		*who = IDL_IDENT(IDL_CONST_DCL(p).ident).str;
		break;
	case IDLN_EXCEPT_DCL:
		*what = "exception";
		*who = IDL_IDENT(IDL_EXCEPT_DCL(p).ident).str;
		break;
	case IDLN_OP_DCL:
		*what = "interface operation";
		*who = IDL_IDENT(IDL_OP_DCL(p).ident).str;
		break;
	case IDLN_MODULE:
		*what = "module";
		*who = IDL_IDENT(IDL_MODULE(p).ident).str;
		break;
	case IDLN_FORWARD_DCL:
		*what = "forward declaration";
		*who = IDL_IDENT(IDL_FORWARD_DCL(p).ident).str;
		break;
	case IDLN_INTERFACE:
		*what = "interface";
		*who = IDL_IDENT(IDL_INTERFACE(p).ident).str;
		break;
	default:
		g_warning("Node type: %s\n", IDL_NODE_TYPE_NAME(p));
		*what = "unknown (internal error)";
		break;
	}

	return dienow;
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
		printf("IDL integer: " IDL_SB10_FMT "\n", IDL_INTEGER(p).value);
		break;
		
	case IDLN_STRING:
		printf("IDL string: %s\n", IDL_STRING(p).value);
		break;
		
	case IDLN_CHAR:
		printf("IDL char: %s\n", IDL_CHAR(p).value);
		break;
		
	case IDLN_FIXED:
		printf("IDL fixed: %s\n", IDL_FIXED(p).value);
		break;
		
	case IDLN_FLOAT:
		printf("IDL float: %g\n", IDL_FLOAT(p).value);
		break;
		
	case IDLN_BOOLEAN:
		printf("IDL boolean: %s\n",
		       IDL_BOOLEAN(p).value ? "TRUE" : "FALSE");
		break;
		
	case IDLN_IDENT:
		printf("IDL ident: %s (repo_id \"%s\")\n",
		       IDL_IDENT(p).str,
		       IDL_IDENT_REPO_ID(p) ? IDL_IDENT_REPO_ID(p) : "<NONE>");
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
			free(IDL_GENTREE(q)._cur_prefix);
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
			free(IDL_IDENT_REPO_ID(p));
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

static int my_strcmp(const char *a, const char *b)
{
	int rv = strcasecmp(a, b);
	
	if (idl_is_parsing && rv == 0 && strcmp(a, b) != 0) {
		yywarningv("Case mismatch between `%s' and `%s' ", a, b);
		yywarning("(Identifiers should be case-consistent after initial declaration)");
	}

	return rv;
}

static int identcmp(IDL_tree a, IDL_tree b)
{
	assert(IDL_NODE_TYPE(a) == IDLN_IDENT);
	assert(IDL_NODE_TYPE(b) == IDLN_IDENT);
	return my_strcmp(IDL_IDENT(a).str, IDL_IDENT(b).str);
}

/* If insertion was made, return true, else there was a collision */
static gboolean heap_insert_ident(IDL_tree interface_ident, GTree *heap, IDL_tree any)
{
	IDL_tree p;

	assert(any != NULL);
	assert(heap != NULL);

	if ((p = g_tree_lookup(heap, any))) {
		char *newi;
		char *i1, *i2;
		char *what1 = "identifier", *what2 = what1;
		char *who1, *who2;
		IDL_tree q;

		assert(IDL_NODE_TYPE(p) == IDLN_IDENT);

		newi = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(interface_ident), "::", 0);
		i1 = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(p), "::", 0);
		i2 = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(any), "::", 0);

		q = p;
		while (q && (IDL_NODE_TYPE(q) == IDLN_IDENT || IDL_NODE_TYPE(q) == IDLN_LIST))
			q = IDL_NODE_UP(q);
		assert(q != NULL);
		get_node_info(q, &what1, &who1);

		q = any;
		while (q && (IDL_NODE_TYPE(q) == IDLN_IDENT || IDL_NODE_TYPE(q) == IDLN_LIST))
			q = IDL_NODE_UP(q);
		assert(q != NULL);
		get_node_info(q, &what2, &who2);

		yyerrorv("Ambiguous inheritance in interface `%s' from %s `%s' and %s `%s'",
			 newi, what1, i1, what2, i2);

		free(newi); free(i1); free(i2);

		return IDL_FALSE;
	}

	g_tree_insert(heap, any, any);

	return IDL_TRUE;
}

static int is_visited_interface(GHashTable *visited_interfaces, IDL_tree scope)
{
	assert(scope != NULL);
	assert(IDL_NODE_TYPE(scope) == IDLN_GENTREE);
	/* If already visited, do not visit again */
	return g_hash_table_lookup_extended(visited_interfaces, scope, NULL, NULL);
}

static void mark_visited_interface(GHashTable *visited_interfaces, IDL_tree scope)
{
	assert(scope != NULL);
	assert(IDL_NODE_TYPE(scope) == IDLN_GENTREE);
	g_hash_table_insert(visited_interfaces, scope, scope);
}

/* Return true if adds went okay */
static int IDL_ns_load_idents_to_tables(IDL_tree interface_ident, IDL_tree ident_scope,
					GTree *ident_heap, GHashTable *visited_interfaces)
{
	IDL_tree p, q, scope;
	int insert_conflict = 0;

	assert(ident_scope != NULL);
	assert(IDL_NODE_TYPE(ident_scope) == IDLN_IDENT);

	scope = IDL_IDENT_TO_NS(ident_scope);

	if (!scope)
		return IDL_TRUE;

	assert(IDL_NODE_TYPE(scope) == IDLN_GENTREE);
	assert(IDL_GENTREE(scope).data != NULL);
	assert(IDL_NODE_TYPE(IDL_GENTREE(scope).data) == IDLN_IDENT);
	assert(IDL_NODE_UP(IDL_GENTREE(scope).data) != NULL);
	assert(IDL_NODE_TYPE(IDL_NODE_UP(IDL_GENTREE(scope).data)) == IDLN_INTERFACE);

	if (is_visited_interface(visited_interfaces, scope))
		return IDL_TRUE;

	/* Search this namespace */
	for (p = IDL_GENTREE(scope).children; p != NULL; p = IDL_GENTREE(p).siblings) {
		if (IDL_GENTREE(p).data == NULL)
			continue;
		assert(IDL_NODE_TYPE(IDL_GENTREE(p).data) == IDLN_IDENT);

		if (IDL_NODE_UP(IDL_GENTREE(p).data) == NULL)
			continue;
		
		if (!(IDL_NODE_TYPE(IDL_NODE_UP(IDL_GENTREE(p).data)) == IDLN_OP_DCL ||
		      (IDL_NODE_UP(IDL_GENTREE(p).data) &&
		       IDL_NODE_TYPE(IDL_NODE_UP(IDL_NODE_UP(IDL_GENTREE(p).data))) == IDLN_ATTR_DCL)))
			continue;

		if (!heap_insert_ident(interface_ident, ident_heap, IDL_GENTREE(p).data))
			insert_conflict = 1;
	}

	/* If there are inherited namespaces, look in those before giving up */
	q = IDL_GENTREE(scope)._import;
	if (!q)
		insert_conflict = 0;
	else
		assert(IDL_NODE_TYPE(q) == IDLN_LIST);

	/* Add inherited namespace identifiers into heap */
	for (; q != NULL; q = IDL_LIST(q).next) {
		int r;
		
		assert(IDL_LIST(q).data != NULL);
		assert(IDL_NODE_TYPE(IDL_LIST(q).data) == IDLN_IDENT);
		assert(IDL_IDENT_TO_NS(IDL_LIST(q).data) != NULL);
		assert(IDL_NODE_TYPE(IDL_IDENT_TO_NS(IDL_LIST(q).data)) == IDLN_GENTREE);
		assert(IDL_NODE_TYPE(IDL_NODE_UP(IDL_LIST(q).data)) == IDLN_INTERFACE);
		
		if (!(r = IDL_ns_load_idents_to_tables(interface_ident, IDL_GENTREE(q).data,
						       ident_heap, visited_interfaces)))
			insert_conflict = 1;
	}
	
	mark_visited_interface(visited_interfaces, scope);

	return insert_conflict == 0;
}

static int IDL_ns_check_for_ambiguous_inheritance(IDL_tree interface_ident, IDL_tree p)
{
	/* We use a sorted heap to check for namespace collisions,
	   since we must do case-insensitive collision checks.
	   visited_interfaces is a hash of visited interface nodes, so
	   we only visit common ancestors once. */
	GTree *ident_heap;
	GHashTable *visited_interfaces;
	int is_ambiguous = 0;

	if (!p)
		return 0;

	ident_heap = g_tree_new((GCompareFunc)identcmp);
	visited_interfaces = g_hash_table_new(g_direct_hash, g_direct_equal);

	assert(IDL_NODE_TYPE(p) == IDLN_LIST);
	while (p) {
		char *s;

		s = IDL_ns_ident_to_qstring(IDL_IDENT_TO_NS(IDL_LIST(p).data), "::", 0);
		if (!s)
			break;

		if (!IDL_ns_load_idents_to_tables(interface_ident, IDL_LIST(p).data,
						  ident_heap, visited_interfaces))
			is_ambiguous = 1;

		free(s);
		p = IDL_LIST(p).next;
	}

	g_tree_destroy(ident_heap);
	g_hash_table_destroy(visited_interfaces);

	return is_ambiguous;
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

int IDL_ns_prefix(IDL_ns ns, const char *s)
{
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

	if (IDL_GENTREE(IDL_NS(ns).current)._cur_prefix)
		free(IDL_GENTREE(IDL_NS(ns).current)._cur_prefix);

	IDL_GENTREE(IDL_NS(ns).current)._cur_prefix = r;

	return IDL_TRUE;
}

IDL_tree IDL_ns_resolve_this_scope_ident(IDL_ns ns, IDL_tree scope, IDL_tree ident)
{
	IDL_tree p, q;
	
	IDL_NS_ASSERTS;

	p = scope;

	while (p != NULL) {

		q = IDL_ns_lookup_this_scope(ns, p, ident);
		
		if (q != NULL)
			return q;

		p = IDL_NODE_UP(p);
	}

	return p;
}

IDL_tree IDL_ns_resolve_ident(IDL_ns ns, IDL_tree ident)
{
	return IDL_ns_resolve_this_scope_ident(ns, IDL_NS(ns).current, ident);
}

IDL_tree IDL_ns_lookup_this_scope(IDL_ns ns, IDL_tree scope, IDL_tree ident)
{
	IDL_tree p, q;
	
	IDL_NS_ASSERTS;

	if (scope == NULL)
		return NULL;
	
	assert(IDL_NODE_TYPE(scope) == IDLN_GENTREE);

	/* Search this namespace */
	for (p = IDL_GENTREE(scope).children; p != NULL; p = IDL_GENTREE(p).siblings) {
		if (IDL_GENTREE(p).data == NULL)
			continue;
		assert(IDL_NODE_TYPE(IDL_GENTREE(p).data) == IDLN_IDENT);
		if (identcmp(IDL_GENTREE(p).data, ident) == 0)
			return p;
	}

	/* If there are inherited namespaces, look in those before giving up */
	q = IDL_GENTREE(scope)._import;
	if (!q)
		return NULL;

	assert(IDL_NODE_TYPE(q) == IDLN_LIST);
	for (; q != NULL; q = IDL_LIST(q).next) {
		IDL_tree r;
		
		assert(IDL_LIST(q).data != NULL);
		assert(IDL_NODE_TYPE(IDL_LIST(q).data) == IDLN_IDENT);
		assert(IDL_IDENT_TO_NS(IDL_LIST(q).data) != NULL);
		assert(IDL_NODE_TYPE(IDL_IDENT_TO_NS(IDL_LIST(q).data)) == IDLN_GENTREE);

		/* Search inherited namespaces */
		for (p = IDL_GENTREE(IDL_IDENT_TO_NS(IDL_LIST(q).data)).children;
		     p != NULL; p = IDL_GENTREE(p).siblings) {
			if (IDL_GENTREE(p).data == NULL)
				continue;
			assert(IDL_NODE_TYPE(IDL_GENTREE(p).data) == IDLN_IDENT);
			if (identcmp(IDL_GENTREE(p).data, ident) == 0)
				return p;
		}

		/* Search up one level */
		if (IDL_NODE_TYPE(IDL_NODE_UP(IDL_LIST(q).data)) == IDLN_INTERFACE &&
		    (r = IDL_ns_lookup_this_scope(ns, IDL_IDENT_TO_NS(IDL_LIST(q).data), ident)))
			return r;
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

	/* Make default repository ID */
	IDL_IDENT_REPO_ID(ident) = IDL_ns_ident_make_repo_id(idl_ns, p, NULL, NULL, NULL);

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
	char *cmd;
#ifdef HAVE_CPP_PIPE_STDIN
	char *fmt = CPP_PROGRAM " - %s < \"%s\" 2>/dev/null";
#else
	char *fmt = CPP_PROGRAM " -I- -I%s %s \"%s\" 2>/dev/null";
	char *s, *tmpfilename;
	char cwd[2048];
	gchar *linkto;
#endif
	int rv;

	if (!filename ||
	    !tree ||
	    (tree == NULL && ns != NULL)) {
		errno = EINVAL;
		return -1;
	}

	if (access(filename, R_OK))
		return -1;

#ifdef HAVE_CPP_PIPE_STDIN
	cmd = (char *)malloc(strlen(filename) + 
			     (cpp_args ? strlen(cpp_args) : 0) +
			     strlen(fmt) - 4 + 1);
	if (!cmd) {
		errno = ENOMEM;
		return -1;
	}

	sprintf(cmd, fmt, cpp_args ? cpp_args : "", filename);
#else
	s = tmpnam(NULL);
	if (s == NULL)
		return -1;

	if (!getcwd(cwd, sizeof(cwd)))
		return -1;

	if (*filename == '/') {
		linkto = strdup(filename);
	} else {
		linkto = (char *)malloc(strlen(cwd) + strlen(filename) + 2);
		if (!linkto) {
			errno = ENOMEM;
			return -1;
		}
		strcpy(linkto, cwd);
		strcat(linkto, "/");
		strcat(linkto, filename);
	}

	tmpfilename = (char *)malloc(strlen(s) + 3);
	if (!tmpfilename) {
		free(linkto);
		errno = ENOMEM;
		return -1;
	}
	strcpy(tmpfilename, s);
	strcat(tmpfilename, ".c");
	if (symlink(linkto, tmpfilename) < 0) {
		free(linkto);
		free(tmpfilename);
		return -1;
	}
	free(linkto);

	cmd = (char *)malloc(strlen(tmpfilename) + 
			     strlen(cwd) +
			     (cpp_args ? strlen(cpp_args) : 0) +
			     strlen(fmt) - 6 + 1);
	if (!cmd) {
		free(tmpfilename);
		errno = ENOMEM;
		return -1;
	}

	sprintf(cmd, fmt, cwd, cpp_args ? cpp_args : "", tmpfilename);
#endif

	input = popen(cmd, "r");
	free(cmd);

	if (input == NULL || ferror(input)) {
#ifndef HAVE_CPP_PIPE_STDIN
		free(tmpfilename);
#endif
		return IDL_ERROR;
	}

	__IDL_in = input;
	idl_msgcb = cb;
	flags = parse_flags;
	idl_ns = IDL_ns_new();
	idl_is_parsing = IDL_TRUE;
	idl_is_okay = IDL_TRUE;
	__IDL_lex_init();
	__IDL_real_filename = filename;
#ifndef HAVE_CPP_PIPE_STDIN
	__IDL_tmp_filename = tmpfilename;
#endif
	__IDL_cur_filename = strdup(filename);
	rv = yyparse();
	idl_is_parsing = IDL_FALSE;
	__IDL_lex_cleanup();
	__IDL_real_filename = NULL;
#ifndef HAVE_CPP_PIPE_STDIN
	__IDL_tmp_filename = NULL;
#endif
	idl_msgcb = NULL;
	pclose(input);
#ifndef HAVE_CPP_PIPE_STDIN
	unlink(tmpfilename);
	free(tmpfilename);
#endif

	if (rv != 0 || !idl_is_okay) {
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
	IDL_tree p;

	if (!a)
		return NULL;

	p = IDL_list_new(a);

	IDL_LIST(p)._tail = p;

	return p;
}

static IDL_tree list_chain(IDL_tree a, IDL_tree b)
{
	IDL_tree p;

	if (!b)
		return a;

	if (!a)
		return list_start(b);

	p = IDL_list_new(b);

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

	/* Make sure the up node points to the interface */
	if (ident && IDL_NODE_UP(ident) &&
	    IDL_NODE_TYPE(IDL_NODE_UP(ident)) != IDLN_INTERFACE)
		IDL_NODE_UP(ident) = NULL;
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

	assign_up_node(p, ident);
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
		yyerror("Invalid mix of types in constant expression");
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
			yyerror("Invalid operation on non-integer value");
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
			yyerror("Operand to complement must be integer");
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
			yyerror("Divide by zero in constant expression");
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
/* If we ever use something like gmp we could do fixed constant
   evaluation here... */
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
			yyerror("Divide by zero in constant expression");
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
			yyerror("Divide by zero in constant expression");
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
