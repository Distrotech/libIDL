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

struct _IDL_tree_node {
	struct _IDL_tree_node *_s, *_c;
	struct _IDL_tree_node *_s_tail, *_c_tail;
};

typedef struct _IDL_tree_node		IDL_tree_node;
typedef struct _IDL_tree_node *		IDL_tree;

#define IDL_SUCCESS			0
#define IDL_ERROR			1

extern int				IDL_parse_filename(const char *filename,
							   const char *cpp_args,
							   IDL_tree *tree);
extern const char *			IDL_get_last_error(void);
extern void				IDL_free(IDL_tree *tree);

#ifdef __cplusplus
}
#endif

#endif /* __IDL_H */
