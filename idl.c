/**************************************************************************

    idl.c

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "idl.h"
#include "rename.h"
#include "util.h"

int IDL_parse_filename(const char *filename, const char *cpp_args, IDL_tree *tree)
{
	extern void __idl_lex_init(void);
	extern void __idl_lex_cleanup(void);
	extern IDL_tree __idl_root;
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
	__idl_lex_init();
	rv = yyparse();
	__idl_lex_cleanup();
	pclose(input);

	if (rv != 0)
		return IDL_ERROR;

	*tree == __idl_root;

	return IDL_SUCCESS;
}

const char *IDL_get_last_error(void)
{
	return "IDL Success\n";
}

void IDL_free(IDL_tree *tree)
{
}
