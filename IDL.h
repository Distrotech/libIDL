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
IDL_tree				IDL_ident_get(IDL_tree *table, char *s_ident, int add, int *added);

struct _IDL_TYPE_INTEGER {
	unsigned i_signed:1;
	unsigned i_type:2;
#define	IDL_INTEGER_TYPE_SHORT		0U
#define	IDL_INTEGER_TYPE_INT		1U
#define	IDL_INTEGER_TYPE_LONG		2U
#define	IDL_INTEGER_TYPE_LONGLONG	3U
};
#define IDL_TYPE_INTEGER(a)		((a)->u.idl_type_integer)
IDL_tree				IDL_type_integer_new(unsigned i_signed, unsigned i_type);

struct _IDL_TYPE_DCL {
	IDL_tree type;
	IDL_tree dcls;
};
#define IDL_TYPE_DCL(a)			((a)->u.idl_type_dcl)
IDL_tree				IDL_type_dcl_new(IDL_tree type, IDL_tree dcls);

typedef enum {
	IDLN_NONE,
	IDLN_LIST,
	IDLN_STRING,
	IDLN_IDENT,
	IDLN_TYPE_DCL,
	IDLN_TYPE_INTEGER,
} IDL_tree_type;

struct _IDL_tree_node {
	IDL_tree_type type;
	union {
		struct _IDL_LIST idl_list;
		struct _IDL_STRING idl_string;
		struct _IDL_IDENT idl_ident;
		struct _IDL_TYPE_DCL idl_type_dcl;
		struct _IDL_TYPE_INTEGER idl_type_integer;
	} u;
};
#define IDL_NODE_TYPE(a)		((a)->type)

#define IDL_FALSE			0
#define IDL_TRUE			1

#define IDL_SUCCESS			0
#define IDL_ERROR			1
#define IDL_WARNING			2

typedef int				(*IDL_callback)(int level, int num, int line,
							const char *filename, const char *s);

extern int				IDL_parse_filename(const char *filename,
							   const char *cpp_args,
							   IDL_callback cb,
							   IDL_tree *tree,
							   IDL_tree *symtab);
extern const char *			IDL_get_last_error(void);
extern void				IDL_root_free(IDL_tree root);
extern void				IDL_symtab_free(IDL_tree symtab);

#ifdef __cplusplus
}
#endif

#endif /* __IDL_H */
