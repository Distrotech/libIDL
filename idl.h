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

#ifdef __cplusplus
extern "C" {
#endif

#define IDL_FALSE			0
#define IDL_TRUE			1

#define IDL_SUCCESS			0
#define IDL_ERROR			1
#define IDL_WARNING			2

typedef struct _IDL_tree_node 		IDL_tree_node;
typedef struct _IDL_tree_node *		IDL_tree;

struct _IDL_LIST {
	IDL_tree data;
	IDL_tree next;
	IDL_tree _tail;
};
#define IDL_LIST(a)			((a)->u.idl_list)
IDL_tree				IDL_list_new(IDL_tree data);

struct _IDL_STRING {
	char *str;
};
#define IDL_STRING(a)			((a)->u.idl_string)
IDL_tree				IDL_string_new(char *s);

struct _IDL_IDENT {
	char *str;
};
#define IDL_IDENT(a)			((a)->u.idl_ident)
IDL_tree				IDL_ident_get(IDL_tree *table, char *s_ident,
						      int add, int *added);

struct _IDL_TYPE_FLOAT {
	unsigned f_type			: 2;
#define	IDL_FLOAT_TYPE_FLOAT		0U
#define	IDL_FLOAT_TYPE_DOUBLE		1U
#define	IDL_FLOAT_TYPE_LONGDOUBLE	2U
};
#define IDL_TYPE_FLOAT(a)		((a)->u.idl_type_float)
IDL_tree				IDL_type_float_new(unsigned f_type);

struct _IDL_TYPE_INTEGER {
	unsigned f_signed		: 1;
	unsigned f_type			: 2;
#define	IDL_INTEGER_TYPE_SHORT		0U
#define	IDL_INTEGER_TYPE_LONG		1U
#define	IDL_INTEGER_TYPE_LONGLONG	2U
};
#define IDL_TYPE_INTEGER(a)		((a)->u.idl_type_integer)
IDL_tree				IDL_type_integer_new(unsigned f_signed, unsigned f_type);

IDL_tree				IDL_type_char_new(void);
IDL_tree				IDL_type_wide_char_new(void);
IDL_tree				IDL_type_boolean_new(void);
IDL_tree				IDL_type_octet_new(void);
IDL_tree				IDL_type_any_new(void);
IDL_tree				IDL_type_object_new(void);

struct _IDL_TYPE_ENUM {
	IDL_tree ident;
	IDL_tree enumerator_list;
};
#define IDL_TYPE_ENUM(a)		((a)->u.idl_type_enum)
IDL_tree				IDL_type_enum_new(IDL_tree ident,
							  IDL_tree enumerator_list);

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

typedef enum {
	IDLN_NONE,
	IDLN_LIST,
	IDLN_STRING,
	IDLN_IDENT,

	IDLN_TYPE_DCL,
	IDLN_TYPE_INTEGER,
	IDLN_TYPE_FLOAT,
	IDLN_TYPE_CHAR,
	IDLN_TYPE_WIDE_CHAR,
	IDLN_TYPE_BOOLEAN,
	IDLN_TYPE_OCTET,
	IDLN_TYPE_ANY,
	IDLN_TYPE_OBJECT,
	IDLN_TYPE_ENUM,
	IDLN_TYPE_STRUCT,
	IDLN_TYPE_UNION,
	IDLN_MEMBER,
	
} IDL_tree_type;

struct _IDL_tree_node {
	IDL_tree_type type;
	union {
		struct _IDL_LIST idl_list;
		struct _IDL_STRING idl_string;
		struct _IDL_IDENT idl_ident;

		struct _IDL_TYPE_DCL idl_type_dcl;
		struct _IDL_TYPE_FLOAT idl_type_float;
		struct _IDL_TYPE_INTEGER idl_type_integer;
		struct _IDL_TYPE_ENUM idl_type_enum;

		struct _IDL_TYPE_STRUCT idl_type_struct;
		struct _IDL_TYPE_UNION idl_type_union;
		struct _IDL_MEMBER idl_member;
	} u;
};
#define IDL_NODE_TYPE(a)		((a)->type)

typedef int				(*IDL_callback)(int level, int num, int line,
							const char *filename, const char *s);

extern int				IDL_parse_filename(const char *filename, const char *cpp_args,
							   IDL_callback cb,
							   IDL_tree *tree, IDL_tree *symtab);
extern void				IDL_root_free(IDL_tree root);
extern void				IDL_symtab_free(IDL_tree symtab);

#ifdef __cplusplus
}
#endif

#endif /* __IDL_H */
