/**************************************************************************

    util.c

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
#include <string.h>
#include "util.h"

#ifndef HAVE_STRDUP
char *strdup(const char *s)
{
	char *p;
	if (!s) return NULL;
	p = (char *)malloc(strlen(s) + 1);
	strcpy(p, s);
	return p;
}
#endif

const char *IDL_tree_type_names[] = {
"IDLN_NONE",
"IDLN_ANY",
"IDLN_LIST",
"IDLN_GENTREE",
"IDLN_INTEGER",
"IDLN_STRING",
"IDLN_WIDE_STRING",
"IDLN_CHAR",
"IDLN_WIDE_CHAR",
"IDLN_FIXED",
"IDLN_FLOAT",
"IDLN_BOOLEAN",
"IDLN_IDENT",
"IDLN_TYPE_DCL",
"IDLN_CONST_DCL",
"IDLN_EXCEPT_DCL",
"IDLN_ATTR_DCL",
"IDLN_OP_DCL",
"IDLN_PARAM_DCL",
"IDLN_FORWARD_DCL",
"IDLN_TYPE_INTEGER",
"IDLN_TYPE_FLOAT",
"IDLN_TYPE_FIXED",
"IDLN_TYPE_CHAR",
"IDLN_TYPE_WIDE_CHAR",
"IDLN_TYPE_STRING",
"IDLN_TYPE_WIDE_STRING",
"IDLN_TYPE_BOOLEAN",
"IDLN_TYPE_OCTET",
"IDLN_TYPE_ANY",
"IDLN_TYPE_OBJECT",
"IDLN_TYPE_ENUM",
"IDLN_TYPE_SEQUENCE",
"IDLN_TYPE_ARRAY",
"IDLN_TYPE_STRUCT",
"IDLN_TYPE_UNION",
"IDLN_MEMBER",
"IDLN_CASE_STMT",
"IDLN_INTERFACE",
"IDLN_MODULE",
"IDLN_BINOP",
"IDLN_UNARYOP"
};


/*
 * Local variables:
 * mode: C
 * c-basic-offset: 8
 * tab-width: 8
 * indent-tabs-mode: t
 * End:
 */
