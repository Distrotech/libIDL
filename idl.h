/**************************************************************************

    idl.h

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

#ifndef __IDL_H
#define __IDL_H

#include <wchar.h>

#ifdef __cplusplus
extern "C" {
#endif

/* miscellaneous constants */
#define IDL_FALSE			0
#define IDL_TRUE			1

#define IDL_SUCCESS			0
#define IDL_ERROR			1
#define IDL_WARNING			2

/* parse flags for IDL_parse_filename */
#define IDLF_EVAL_CONST			(1UL << 0)
#define IDLF_ROOT_APPEND		(1UL << 1)
#define IDLF_NS_APPEND			(1UL << 2)

typedef struct _IDL_tree_node 		IDL_tree_node;
typedef struct _IDL_tree_node *		IDL_tree;

struct _IDL_LIST {
	IDL_tree data;
	IDL_tree next;
	IDL_tree _tail;
};
#define IDL_LIST(a)			((a)->u.idl_list)
IDL_tree				IDL_list_new(IDL_tree data);

struct _IDL_GENTREE {
	IDL_tree parent;
	IDL_tree data;
	IDL_tree siblings, _siblings_tail;
	IDL_tree children;
};
#define IDL_GENTREE(a)			((a)->u.idl_gentree)
IDL_tree				IDL_gentree_new(IDL_tree parent,
							IDL_tree data);
IDL_tree				IDL_gentree_chain_sibling(IDL_tree from,
								  IDL_tree data);
IDL_tree				IDL_gentree_chain_child(IDL_tree from,
								IDL_tree data);


struct _IDL_INTEGER {
	long value;
};
#define IDL_INTEGER(a)			((a)->u.idl_integer)
IDL_tree				IDL_integer_new(long value);

struct _IDL_STRING {
	char *value;
};
#define IDL_STRING(a)			((a)->u.idl_string)
IDL_tree				IDL_string_new(char *value);

struct _IDL_WIDE_STRING {
	wchar_t *value;
};
#define IDL_WIDE_STRING(a)		((a)->u.idl_wide_string)
IDL_tree				IDL_wide_string_new(wchar_t *value);

struct _IDL_CHAR {
	char *value;
};
#define IDL_CHAR(a)			((a)->u.idl_char)
IDL_tree				IDL_char_new(char *value);

struct _IDL_WIDE_CHAR {
	wchar_t *value;
};
#define IDL_WIDE_CHAR(a)		((a)->u.idl_wide_char)
IDL_tree				IDL_wide_char_new(wchar_t *value);

struct _IDL_FIXED {
	double value;
};
#define IDL_FIXED(a)			((a)->u.idl_fixed)
IDL_tree				IDL_fixed_new(double value);

struct _IDL_FLOAT {
	double value;
};
#define IDL_FLOAT(a)			((a)->u.idl_float)
IDL_tree				IDL_float_new(double value);

struct _IDL_BOOLEAN {
	unsigned value;
};
#define IDL_BOOLEAN(a)			((a)->u.idl_boolean)
IDL_tree				IDL_boolean_new(unsigned value);

struct _IDL_IDENT {
	char *str;
	IDL_tree data;
	int _refs;
};
#define IDL_IDENT(a)			((a)->u.idl_ident)
IDL_tree				IDL_ident_new(char *str);

struct _IDL_TYPE_FLOAT {
	enum IDL_float_type {
		IDL_FLOAT_TYPE_FLOAT,
		IDL_FLOAT_TYPE_DOUBLE,
		IDL_FLOAT_TYPE_LONGDOUBLE
	} f_type;
};
#define IDL_TYPE_FLOAT(a)		((a)->u.idl_type_float)
IDL_tree				IDL_type_float_new(enum IDL_float_type f_type);

struct _IDL_TYPE_FIXED {
	IDL_tree positive_int_const;
	IDL_tree integer_lit;
};
#define IDL_TYPE_FIXED(a)		((a)->u.idl_type_fixed)
IDL_tree				IDL_type_fixed_new(IDL_tree positive_int_const,
							   IDL_tree integer_lit);

struct _IDL_TYPE_INTEGER {
	unsigned f_signed		: 1;
	enum IDL_integer_type {
		IDL_INTEGER_TYPE_SHORT,
		IDL_INTEGER_TYPE_LONG,
		IDL_INTEGER_TYPE_LONGLONG
	} f_type;
};
#define IDL_TYPE_INTEGER(a)		((a)->u.idl_type_integer)
IDL_tree				IDL_type_integer_new(unsigned f_signed,
							     enum IDL_integer_type f_type);

IDL_tree				IDL_type_char_new(void);
IDL_tree				IDL_type_wide_char_new(void);
IDL_tree				IDL_type_boolean_new(void);
IDL_tree				IDL_type_octet_new(void);
IDL_tree				IDL_type_any_new(void);
IDL_tree				IDL_type_object_new(void);

struct _IDL_TYPE_STRING {
	IDL_tree positive_int_const;
};
#define IDL_TYPE_STRING(a)		((a)->u.idl_type_string)
IDL_tree				IDL_type_string_new(IDL_tree positive_int_const);

struct _IDL_TYPE_WIDE_STRING {
	IDL_tree positive_int_const;
};
#define IDL_TYPE_WIDE_STRING(a)		((a)->u.idl_type_wide_string)
IDL_tree				IDL_type_wide_string_new(IDL_tree positive_int_const);

struct _IDL_TYPE_ENUM {
	IDL_tree ident;
	IDL_tree enumerator_list;
};
#define IDL_TYPE_ENUM(a)		((a)->u.idl_type_enum)
IDL_tree				IDL_type_enum_new(IDL_tree ident,
							  IDL_tree enumerator_list);

struct _IDL_TYPE_ARRAY {
	IDL_tree ident;
	IDL_tree size_list;
};
#define IDL_TYPE_ARRAY(a)		((a)->u.idl_type_array)
IDL_tree				IDL_type_array_new(IDL_tree ident,
							   IDL_tree size_list);

struct _IDL_TYPE_SEQUENCE {
	IDL_tree simple_type_spec;
	IDL_tree positive_int_const;
};
#define IDL_TYPE_SEQUENCE(a)		((a)->u.idl_type_sequence)
IDL_tree				IDL_type_sequence_new(IDL_tree simple_type_spec,
							      IDL_tree positive_int_const);

struct _IDL_TYPE_STRUCT {
	IDL_tree ident;
	IDL_tree member_list;
};
#define IDL_TYPE_STRUCT(a)		((a)->u.idl_type_struct)
IDL_tree				IDL_type_struct_new(IDL_tree ident,
							    IDL_tree member_list);

struct _IDL_TYPE_UNION {
	IDL_tree ident;
	IDL_tree switch_type_spec;
	IDL_tree switch_body;
};
#define IDL_TYPE_UNION(a)		((a)->u.idl_type_union)
IDL_tree				IDL_type_union_new(IDL_tree ident,
							   IDL_tree switch_type_spec,
							   IDL_tree switch_body);
struct _IDL_MEMBER {
	IDL_tree type_spec;
	IDL_tree dcls;
};
#define IDL_MEMBER(a)			((a)->u.idl_member)
IDL_tree				IDL_member_new(IDL_tree type_spec, IDL_tree dcls);


struct _IDL_TYPE_DCL {
	IDL_tree type_spec;
	IDL_tree dcls;
};
#define IDL_TYPE_DCL(a)			((a)->u.idl_type_dcl)
IDL_tree				IDL_type_dcl_new(IDL_tree type_spec, IDL_tree dcls);

struct _IDL_CONST_DCL {
	IDL_tree const_type;
	IDL_tree ident;
	IDL_tree const_exp;
};
#define IDL_CONST_DCL(a)		((a)->u.idl_const_dcl)
IDL_tree				IDL_const_dcl_new(IDL_tree const_type, IDL_tree ident, IDL_tree const_exp);

struct _IDL_EXCEPT_DCL {
	IDL_tree ident;
	IDL_tree members;
};
#define IDL_EXCEPT_DCL(a)		((a)->u.idl_except_dcl)
IDL_tree				IDL_except_dcl_new(IDL_tree ident, IDL_tree members);

struct _IDL_ATTR_DCL {
	unsigned f_readonly		: 1;
	IDL_tree param_type_spec;
	IDL_tree simple_declarations;
};
#define IDL_ATTR_DCL(a)			((a)->u.idl_attr_dcl)
IDL_tree				IDL_attr_dcl_new(unsigned f_readonly,
							 IDL_tree param_type_spec,
							 IDL_tree simple_declarations);

struct _IDL_OP_DCL {
	unsigned f_oneway		: 1;
	IDL_tree op_type_spec;
	IDL_tree ident;
	IDL_tree parameter_dcls;
	IDL_tree raises_expr;
	IDL_tree context_expr;
};
#define IDL_OP_DCL(a)			((a)->u.idl_op_dcl)
IDL_tree				IDL_op_dcl_new(unsigned f_oneway,
						       IDL_tree op_type_spec,
						       IDL_tree ident,
						       IDL_tree parameter_dcls,
						       IDL_tree raises_expr,
						       IDL_tree context_expr);

struct _IDL_PARAM_DCL {
	enum IDL_param_attr {
		IDL_PARAM_IN,
		IDL_PARAM_OUT,
		IDL_PARAM_INOUT
	} attr;
	IDL_tree param_type_spec;
	IDL_tree simple_declarator;
};
#define IDL_PARAM_DCL(a)		((a)->u.idl_param_dcl)
IDL_tree				IDL_param_dcl_new(enum IDL_param_attr attr,
							  IDL_tree param_type_spec,
							  IDL_tree simple_declarator);

struct _IDL_CASE_LABEL {
	unsigned f_default		: 1;
	IDL_tree const_exp;
};
#define IDL_CASE_LABEL(a)		((a)->u.idl_case_label)
IDL_tree				IDL_case_label_new(IDL_tree const_exp);

struct _IDL_INTERFACE {
	IDL_tree ident;
	IDL_tree inheritence_spec;
	IDL_tree body;
};
#define IDL_INTERFACE(a)		((a)->u.idl_interface)
IDL_tree				IDL_interface_new(IDL_tree ident, IDL_tree inheritence_spec, IDL_tree body);

struct _IDL_FORWARD_DCL {
	IDL_tree ident;
};
#define IDL_FORWARD_DCL(a)		((a)->u.idl_forward_dcl)
IDL_tree				IDL_forward_dcl_new(IDL_tree ident);

struct _IDL_MODULE {
	IDL_tree ident;
	IDL_tree definition_list;
};
#define IDL_MODULE(a)			((a)->u.idl_module)
IDL_tree				IDL_module_new(IDL_tree ident, IDL_tree definition_list);

struct _IDL_BINOP {
	enum IDL_binop {
		IDL_BINOP_OR,
		IDL_BINOP_XOR,
		IDL_BINOP_AND,
		IDL_BINOP_SHR,
		IDL_BINOP_SHL,
		IDL_BINOP_ADD,
		IDL_BINOP_SUB,
		IDL_BINOP_MULT,
		IDL_BINOP_DIV,
		IDL_BINOP_MOD
	} op;
	IDL_tree left, right;
};
#define IDL_BINOP(a)			((a)->u.idl_binop)
IDL_tree				IDL_binop_new(enum IDL_binop op, IDL_tree left, IDL_tree right);

struct _IDL_UNARYOP {
	enum IDL_unaryop {
		IDL_UNARYOP_PLUS,
		IDL_UNARYOP_MINUS,
		IDL_UNARYOP_COMPLEMENT
	} op;
	IDL_tree operand;
};
#define IDL_UNARYOP(a)			((a)->u.idl_unaryop)
IDL_tree				IDL_unaryop_new(enum IDL_unaryop op, IDL_tree operand);

typedef enum {
	IDLN_NONE,
	IDLN_LIST,
	IDLN_GENTREE,
	IDLN_INTEGER,
	IDLN_STRING,
	IDLN_WIDE_STRING,
	IDLN_CHAR,
	IDLN_WIDE_CHAR,
	IDLN_FIXED,
	IDLN_FLOAT,
	IDLN_BOOLEAN,
	IDLN_IDENT,
	IDLN_TYPE_DCL,
	IDLN_CONST_DCL,
	IDLN_EXCEPT_DCL,
	IDLN_ATTR_DCL,
	IDLN_OP_DCL,
	IDLN_PARAM_DCL,
	IDLN_FORWARD_DCL,
	IDLN_TYPE_INTEGER,
	IDLN_TYPE_FLOAT,
	IDLN_TYPE_FIXED,
	IDLN_TYPE_CHAR,
	IDLN_TYPE_WIDE_CHAR,
	IDLN_TYPE_STRING,
	IDLN_TYPE_WIDE_STRING,
	IDLN_TYPE_BOOLEAN,
	IDLN_TYPE_OCTET,
	IDLN_TYPE_ANY,
	IDLN_TYPE_OBJECT,
	IDLN_TYPE_ENUM,
	IDLN_TYPE_SEQUENCE,
	IDLN_TYPE_ARRAY,
	IDLN_TYPE_STRUCT,
	IDLN_TYPE_UNION,
	IDLN_MEMBER,
	IDLN_CASE_LABEL,
	IDLN_INTERFACE,
	IDLN_MODULE,
	IDLN_BINOP,
	IDLN_UNARYOP
} IDL_tree_type;

struct _IDL_tree_node {
	IDL_tree_type type;
	union {
		struct _IDL_LIST idl_list;
		struct _IDL_GENTREE idl_gentree;
		struct _IDL_INTEGER idl_integer;
		struct _IDL_STRING idl_string;
		struct _IDL_WIDE_STRING idl_wide_string;
		struct _IDL_CHAR idl_char;
		struct _IDL_WIDE_CHAR idl_wide_char;
		struct _IDL_FIXED idl_fixed;
		struct _IDL_FLOAT idl_float;
		struct _IDL_BOOLEAN idl_boolean;
		struct _IDL_IDENT idl_ident;
		struct _IDL_TYPE_DCL idl_type_dcl;
		struct _IDL_CONST_DCL idl_const_dcl;
		struct _IDL_EXCEPT_DCL idl_except_dcl;
		struct _IDL_ATTR_DCL idl_attr_dcl;
		struct _IDL_OP_DCL idl_op_dcl;
		struct _IDL_PARAM_DCL idl_param_dcl;
		struct _IDL_FORWARD_DCL idl_forward_dcl;
		struct _IDL_TYPE_FLOAT idl_type_float;
		struct _IDL_TYPE_FIXED idl_type_fixed;
		struct _IDL_TYPE_INTEGER idl_type_integer;
		struct _IDL_TYPE_ENUM idl_type_enum;
		struct _IDL_TYPE_STRING idl_type_string;
		struct _IDL_TYPE_WIDE_STRING idl_type_wide_string;
		struct _IDL_TYPE_SEQUENCE idl_type_sequence;
		struct _IDL_TYPE_ARRAY idl_type_array;
		struct _IDL_TYPE_STRUCT idl_type_struct;
		struct _IDL_TYPE_UNION idl_type_union;
		struct _IDL_MEMBER idl_member;
		struct _IDL_CASE_LABEL idl_case_label;
		struct _IDL_INTERFACE idl_interface;
		struct _IDL_MODULE idl_module;
		struct _IDL_BINOP idl_binop;
		struct _IDL_UNARYOP idl_unaryop;
	} u;
};
#define IDL_NODE_TYPE(a)		((a)->type)

typedef struct _IDL_ns *		IDL_ns;

struct _IDL_ns {
	IDL_tree global;
	IDL_tree file;
	IDL_tree current;
};
#define IDL_NS(a)			(*a)

typedef int				(*IDL_callback)(int level, int num, int line,
							const char *filename, const char *s);

extern int				IDL_parse_filename(const char *filename,
							   const char *cpp_args,
							   IDL_callback cb,
							   IDL_tree *tree, IDL_ns *ns,
							   unsigned long parse_flags);

extern void				IDL_tree_free(IDL_tree root);

extern char *				IDL_do_escapes(const char *s);

extern IDL_ns				IDL_ns_new(void);

extern void				IDL_ns_free(IDL_ns ns);

extern IDL_tree				IDL_ns_prefix(IDL_ns ns, IDL_tree prefix);

extern IDL_tree				IDL_ns_resolve_ident(IDL_ns ns, IDL_tree ident);

extern IDL_tree				IDL_ns_lookup_this_scope(IDL_ns ns, IDL_tree scope, IDL_tree ident);

extern IDL_tree				IDL_ns_lookup_cur_scope(IDL_ns ns, IDL_tree ident);

extern IDL_tree				IDL_ns_place_new(IDL_ns ns, IDL_tree ident);

extern IDL_tree				IDL_ns_push_scope_new(IDL_ns ns, IDL_tree ident);

extern IDL_tree				IDL_ns_push_scope_prev(IDL_ns ns, IDL_tree ident);

extern IDL_tree				IDL_ns_push_scope_new_or_prev(IDL_ns ns, IDL_tree ident);

extern IDL_tree				IDL_ns_pop_scope(IDL_ns ns);

extern IDL_tree				IDL_ns_get_qualified_ident(IDL_ns ns, IDL_tree ident);

#ifdef __cplusplus
}
#endif

#endif /* __IDL_H */
