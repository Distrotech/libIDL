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
	"IDLN_NATIVE",
	"IDLN_CASE_STMT",
	"IDLN_INTERFACE",
	"IDLN_MODULE",
	"IDLN_BINOP",
	"IDLN_UNARYOP",
};

int				__IDL_check_type_casts = FALSE;
#ifndef HAVE_CPP_PIPE_STDIN
char *				__IDL_tmp_filename = NULL;
#endif
const char *			__IDL_real_filename = NULL;
char *				__IDL_cur_filename = NULL;
int				__IDL_cur_line;
GHashTable *			__IDL_filename_hash;
IDL_tree			__IDL_root;
IDL_ns				__IDL_root_ns;
int				__IDL_is_okay;
int				__IDL_is_parsing;
unsigned long			__IDL_flags;
static int			__IDL_nerrors, __IDL_nwarnings;
static IDL_callback		__IDL_msgcb;

/* Case-insensitive version of g_str_hash */
guint IDL_strcase_hash(gconstpointer v)
{
	const char *p;
	guint h = 0, g;
	
	for (p = (char *)v; *p != '\0'; ++p) {
		h = (h << 4) + isupper(*p) ? tolower(*p) : *p;
		if ((g = h & 0xf0000000)) {
			h = h ^ (g >> 24);
			h = h ^ g;
		}
	}

	return h /* % M */;
}

gint IDL_strcase_equal(gconstpointer a, gconstpointer b)
{
	return g_strcasecmp(a, b) == 0;
}

gint IDL_strcase_cmp(gconstpointer a, gconstpointer b)
{
	return g_strcasecmp(a, b);
}

static int my_strcmp(IDL_tree p, IDL_tree q)
{
	const char *a = IDL_IDENT(p).str;
	const char *b = IDL_IDENT(q).str;
	int cmp = IDL_strcase_cmp(a, b);
	
	if (__IDL_is_parsing &&
	    cmp == 0 &&
	    strcmp(a, b) != 0 &&
	    !(IDL_IDENT(p)._flags & IDLF_IDENT_CASE_MISMATCH_HIT ||
	      IDL_IDENT(q)._flags & IDLF_IDENT_CASE_MISMATCH_HIT)) {
		yywarningv(IDL_WARNING1, "Case mismatch between `%s' and `%s' ", a, b);
		yywarning(IDL_WARNING1, "(Identifiers should be case-consistent after initial declaration)");
		IDL_IDENT(p)._flags |= IDLF_IDENT_CASE_MISMATCH_HIT;
		IDL_IDENT(q)._flags |= IDLF_IDENT_CASE_MISMATCH_HIT;
	}

	return cmp;
}

guint IDL_ident_hash(gconstpointer v)
{
	return IDL_strcase_hash(IDL_IDENT((IDL_tree)v).str);
}

gint IDL_ident_equal(gconstpointer a, gconstpointer b)
{
	return my_strcmp((IDL_tree)a, (IDL_tree)b) == 0;
}

gint IDL_ident_cmp(gconstpointer a, gconstpointer b)
{
	return my_strcmp((IDL_tree)a, (IDL_tree)b);
}

const char *IDL_get_libver_string(void)
{
	return VERSION;
}

const char *IDL_get_IDLver_string(void)
{
	return "2.2";
}

static void IDL_tree_optimize(IDL_tree *p, IDL_ns ns)
{
	IDL_tree_process_forward_dcls(p, ns);
	IDL_tree_remove_inhibits(p, ns);
	IDL_tree_remove_empty_modules(p, ns);
}

int IDL_parse_filename(const char *filename, const char *cpp_args,
		       IDL_callback cb, IDL_tree *tree, IDL_ns *ns,
		       unsigned long parse_flags)
{
	extern void __IDL_lex_init(void);
	extern void __IDL_lex_cleanup(void);
	extern int yyparse(void);
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
		linkto = g_strdup(filename);
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

	__IDL_nerrors = __IDL_nwarnings = 0;
	__IDL_in = input;
	__IDL_msgcb = cb;
	__IDL_flags = parse_flags;
	__IDL_root_ns = IDL_ns_new();

	__IDL_is_parsing = TRUE;
	__IDL_is_okay = TRUE;
	__IDL_lex_init();

	__IDL_real_filename = filename;
#ifndef HAVE_CPP_PIPE_STDIN
	__IDL_tmp_filename = tmpfilename;
#endif
	__IDL_filename_hash = g_hash_table_new(g_str_hash, g_str_equal);
	rv = yyparse();
	__IDL_is_parsing = FALSE;
	__IDL_lex_cleanup();
	__IDL_real_filename = NULL;
#ifndef HAVE_CPP_PIPE_STDIN
	__IDL_tmp_filename = NULL;
#endif
	pclose(input);
#ifndef HAVE_CPP_PIPE_STDIN
	unlink(tmpfilename);
	free(tmpfilename);
#endif

	if (__IDL_root != NULL) {
		IDL_tree_optimize(&__IDL_root, __IDL_root_ns);

		if (__IDL_root == NULL)
			yyerror("File empty after optimization");
	}

	__IDL_msgcb = NULL;

	g_hash_table_foreach(__IDL_filename_hash, (GHFunc)g_free, NULL);
	g_hash_table_destroy(__IDL_filename_hash);

	if (rv != 0 || !__IDL_is_okay) {
		if (tree)
			*tree = NULL;

		if (ns)
			*ns = NULL;

		return IDL_ERROR;
	}

	if (__IDL_flags & IDLF_PREFIX_FILENAME)
		IDL_ns_prefix(__IDL_root_ns, filename);

	if (tree)
		*tree = __IDL_root;
	else
		IDL_tree_free(__IDL_root);

	if (ns)
		*ns = __IDL_root_ns;
	else
		IDL_ns_free(__IDL_root_ns);

	return IDL_SUCCESS;
}

void yyerrorl(const char *s, int ofs)
{
	int line = __IDL_cur_line - 1 + ofs;
	gchar *filename = NULL;

	if (__IDL_cur_filename)
		filename = g_basename(__IDL_cur_filename);
	else
		line = -1;

	++__IDL_nerrors;
	__IDL_is_okay = FALSE;
	
	if (__IDL_msgcb)
		(*__IDL_msgcb)(IDL_ERROR, __IDL_nerrors, line, filename, s);
	else {
		if (line > 0)
			fprintf(stderr, "%s:%d: Error: %s\n", filename, line, s);
		else
			fprintf(stderr, "Error: %s\n", s);
	}
}

void yywarningl(int level, const char *s, int ofs)
{
	int line = __IDL_cur_line - 1 + ofs;
	gchar *filename = NULL;

	if (__IDL_cur_filename)
		filename = g_basename(__IDL_cur_filename);
	else
		line = -1;
	
	++__IDL_nwarnings;
	
	if (__IDL_msgcb)
		(*__IDL_msgcb)(level, __IDL_nwarnings, line, filename, s);
	else {
		if (line > 0)
			fprintf(stderr, "%s:%d: Warning: %s\n", filename, line, s);
		else
			fprintf(stderr, "Warning: %s\n", s);
	}
}

void yyerror(const char *s)
{
	yyerrorl(s, 0);
}

void yywarning(int level, const char *s)
{
	yywarningl(level, s, 0);
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

void yywarninglv(int level, const char *fmt, int ofs, ...)
{
	char *msg = (char *)malloc(strlen(fmt) + 2048);
	va_list args;

	va_start(args, ofs);
	vsprintf(msg, fmt, args);
	yywarningl(level, msg, ofs);
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

void yywarningv(int level, const char *fmt, ...)
{
	char *msg = (char *)malloc(strlen(fmt) + 2048);
	va_list args;

	va_start(args, fmt);
	vsprintf(msg, fmt, args);
	yywarning(level, msg);
	va_end(args);
	free(msg);
}

void yyerrornv(IDL_tree p, const char *fmt, ...)
{
	char *file_save = __IDL_cur_filename;
	int line_save = __IDL_cur_line;
	char *msg = (char *)malloc(strlen(fmt) + 2048);
	va_list args;

	if (p == NULL)
		return;
	
	__IDL_cur_filename = p->_file;
	__IDL_cur_line = p->_line;
	va_start(args, fmt);
	vsprintf(msg, fmt, args);
	yyerror(msg);
	va_end(args);
	free(msg);
	__IDL_cur_filename = file_save;
	__IDL_cur_line = line_save;
}

void yywarningnv(IDL_tree p, int level, const char *fmt, ...)
{
	char *file_save = __IDL_cur_filename;
	int line_save = __IDL_cur_line;
	char *msg = (char *)malloc(strlen(fmt) + 2048);
	va_list args;

	if (p == NULL)
		return;
	
	__IDL_cur_filename = p->_file;
	__IDL_cur_line = p->_line;
	va_start(args, fmt);
	vsprintf(msg, fmt, args);
	yywarning(level, msg);
	va_end(args);
	free(msg);
	__IDL_cur_filename = file_save;
	__IDL_cur_line = line_save;
}

int IDL_tree_get_node_info(IDL_tree p, char **what, char **who)
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
	case IDLN_NATIVE:
		*what = "native declaration";
		assert(IDL_NATIVE(p).ident != NULL);
		assert(IDL_NODE_TYPE(IDL_NATIVE(p).ident) == IDLN_IDENT);
		*who = IDL_IDENT(IDL_NATIVE(p).ident).str;
		break;
	case IDLN_LIST:
		if (!IDL_LIST(p).data)
			break;
		assert(IDL_LIST(p)._tail != NULL);
		if (!IDL_LIST(IDL_LIST(p)._tail).data)
			break;
		dienow = IDL_tree_get_node_info(IDL_LIST(IDL_LIST(p)._tail).data, what, who);
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
	IDL_NODE_REFS(p) = 1;

	p->_file = __IDL_cur_filename;
	p->_line = __IDL_cur_line;

	return p;
}

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

IDL_tree IDL_list_new(IDL_tree data)
{
	IDL_tree p = IDL_node_new(IDLN_LIST);
	
	assign_up_node(p, data);
	IDL_LIST(p).data = data;
	IDL_LIST(p)._tail = p;

	return p;
}

IDL_tree IDL_list_concat(IDL_tree orig, IDL_tree append)
{
	if (orig == NULL)
		return append;

	if (append == NULL)
		return orig;

	IDL_LIST(IDL_LIST(orig)._tail).next = append;
	IDL_LIST(append).prev = IDL_LIST(orig)._tail;
	IDL_LIST(orig)._tail = IDL_LIST(append)._tail;

	/* If we ever need correct tails (speedier otherwise) */
#if 0
	{ IDL_tree p;
	for (p = IDL_LIST(orig).next;
	     p && p != append;
	     p = IDL_LIST(p).next)
		IDL_LIST(p)._tail = IDL_LIST(orig)._tail; }
#endif

	return orig;
}

IDL_tree IDL_list_remove(IDL_tree list, IDL_tree p)
{
	IDL_tree new_list = list;

	if (IDL_LIST(p).prev == NULL) {
		assert(list == p);
		new_list = IDL_LIST(p).next;
		if (new_list)
			IDL_LIST(new_list).prev = NULL;
	} else {
		IDL_tree prev = IDL_LIST(p).prev;
		IDL_tree next = IDL_LIST(p).next;
		
		IDL_LIST(prev).next = next;
		if (next)
			IDL_LIST(next).prev = prev;
	}

	IDL_LIST(p).prev = NULL;
	IDL_LIST(p).next = NULL;
	IDL_LIST(p)._tail = p;

	/* Note all tails not updated... */

	return new_list;
}

IDL_tree IDL_gentree_new(GHashFunc hash_func, GCompareFunc key_compare_func, IDL_tree data)
{
	IDL_tree p = IDL_node_new(IDLN_GENTREE);
	
	assign_up_node(p, data);
	IDL_GENTREE(p).data = data;
	IDL_GENTREE(p).hash_func = hash_func;
	IDL_GENTREE(p).key_compare_func = key_compare_func;
	IDL_GENTREE(p).siblings = g_hash_table_new(hash_func, key_compare_func);
	IDL_GENTREE(p).children = g_hash_table_new(hash_func, key_compare_func);

	g_hash_table_insert(IDL_GENTREE(p).siblings, data, p);
	
	return p;
}

IDL_tree IDL_gentree_new_sibling(IDL_tree from, IDL_tree data)
{
	IDL_tree p = IDL_node_new(IDLN_GENTREE);
	
	assign_up_node(p, data);
	IDL_GENTREE(p).data = data;
	IDL_GENTREE(p).hash_func = IDL_GENTREE(from).hash_func;
	IDL_GENTREE(p).key_compare_func = IDL_GENTREE(from).key_compare_func;
	IDL_GENTREE(p).siblings = IDL_GENTREE(from).siblings;
	IDL_GENTREE(p).children = g_hash_table_new(IDL_GENTREE(from).hash_func,
						   IDL_GENTREE(from).key_compare_func);

	return p;
}

IDL_tree IDL_integer_new(IDL_longlong_t value)
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

IDL_tree IDL_native_new(IDL_tree ident)
{
	IDL_tree p = IDL_node_new(IDLN_NATIVE);

	assign_up_node(p, ident);
	IDL_NATIVE(p).ident = ident;
	
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

IDL_tree IDL_check_type_cast(const IDL_tree tree, IDL_tree_type type,
			     const char *file, int line, const char *function)
{
	if (__IDL_check_type_casts) {
		if (tree == NULL) {
			g_warning ("file %s: line %d: (%s) invalid type cast attempt, NULL tree to %s\n",
				   file, line, function,
				   IDL_tree_type_names[type]);
		}
		else if (IDL_NODE_TYPE(tree) != type) {
			g_warning ("file %s: line %d: (%s) expected IDL tree type %s, but got %s\n",
				   file, line, function,
				   IDL_tree_type_names[type], IDL_NODE_TYPE_NAME(tree));
			
		}
	}
	return tree;
}

IDL_tree IDL_gentree_chain_sibling(IDL_tree from, IDL_tree data)
{
	IDL_tree p;

	if (from == NULL)
		return NULL;

	p = IDL_gentree_new_sibling(from, data);
	IDL_NODE_UP(p) = IDL_NODE_UP(from);

	return p;
}

IDL_tree IDL_gentree_chain_child(IDL_tree from, IDL_tree data)
{
	IDL_tree p;

	if (from == NULL)
		return NULL;

	p = IDL_gentree_new(IDL_GENTREE(from).hash_func,
			    IDL_GENTREE(from).key_compare_func,
			    data);
	IDL_NODE_UP(p) = from;

	g_hash_table_insert(IDL_GENTREE(from).children, data, p);

	return p;
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

void IDL_tree_walk_in_order(IDL_tree p, IDL_tree_func tree_func, gpointer user_data)
{
	assert(tree_func != NULL);

	if (!p)
		return;
	
	if (!(*tree_func)(p, user_data))
		return;

	switch (IDL_NODE_TYPE(p)) {
	case IDLN_INTEGER:
	case IDLN_STRING:
	case IDLN_CHAR:
	case IDLN_FIXED:
	case IDLN_FLOAT:
	case IDLN_BOOLEAN:
	case IDLN_IDENT:
	case IDLN_TYPE_WIDE_CHAR:
	case IDLN_TYPE_BOOLEAN:
	case IDLN_TYPE_OCTET:
	case IDLN_TYPE_ANY:
	case IDLN_TYPE_OBJECT:
	case IDLN_TYPE_FLOAT:
	case IDLN_TYPE_INTEGER:
	case IDLN_TYPE_CHAR:
		break;
		
	case IDLN_LIST:
		for (; p; p = IDL_LIST(p).next)
			IDL_tree_walk_in_order(IDL_LIST(p).data, tree_func, user_data);
		break;

	case IDLN_GENTREE:
		g_error("IDLN_GENTREE walk not implemented!");
		break;

	case IDLN_MEMBER:
		IDL_tree_walk_in_order(IDL_MEMBER(p).type_spec, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_MEMBER(p).dcls, tree_func, user_data);
		break;
		
	case IDLN_NATIVE:
		IDL_tree_walk_in_order(IDL_NATIVE(p).ident, tree_func, user_data);
		break;

	case IDLN_TYPE_DCL:
		IDL_tree_walk_in_order(IDL_TYPE_DCL(p).type_spec, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_TYPE_DCL(p).dcls, tree_func, user_data);
		break;

	case IDLN_CONST_DCL:
		IDL_tree_walk_in_order(IDL_CONST_DCL(p).const_type, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_CONST_DCL(p).ident, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_CONST_DCL(p).const_exp, tree_func, user_data);
		break;
		
	case IDLN_EXCEPT_DCL:
		IDL_tree_walk_in_order(IDL_EXCEPT_DCL(p).ident, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_EXCEPT_DCL(p).members, tree_func, user_data);
		break;
		
	case IDLN_ATTR_DCL:
		IDL_tree_walk_in_order(IDL_ATTR_DCL(p).param_type_spec, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_ATTR_DCL(p).simple_declarations, tree_func, user_data);
		break;
		
	case IDLN_OP_DCL:
		IDL_tree_walk_in_order(IDL_OP_DCL(p).op_type_spec, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_OP_DCL(p).ident, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_OP_DCL(p).parameter_dcls, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_OP_DCL(p).raises_expr, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_OP_DCL(p).context_expr, tree_func, user_data);
		break;

	case IDLN_PARAM_DCL:
		IDL_tree_walk_in_order(IDL_PARAM_DCL(p).param_type_spec, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_PARAM_DCL(p).simple_declarator, tree_func, user_data);
		break;

	case IDLN_FORWARD_DCL:
		IDL_tree_walk_in_order(IDL_FORWARD_DCL(p).ident, tree_func, user_data);
		break;
		
	case IDLN_TYPE_FIXED:
		IDL_tree_walk_in_order(IDL_TYPE_FIXED(p).positive_int_const, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_TYPE_FIXED(p).integer_lit, tree_func, user_data);
		break;

	case IDLN_TYPE_STRING:
		IDL_tree_walk_in_order(IDL_TYPE_STRING(p).positive_int_const, tree_func, user_data);
		break;

	case IDLN_TYPE_WIDE_STRING:
		IDL_tree_walk_in_order(IDL_TYPE_WIDE_STRING(p).positive_int_const, tree_func, user_data);
		break;
		
	case IDLN_TYPE_ENUM:
		IDL_tree_walk_in_order(IDL_TYPE_ENUM(p).ident, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_TYPE_ENUM(p).enumerator_list, tree_func, user_data);
		break;

	case IDLN_TYPE_SEQUENCE:
		IDL_tree_walk_in_order(IDL_TYPE_SEQUENCE(p).simple_type_spec, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_TYPE_SEQUENCE(p).positive_int_const, tree_func, user_data);
		break;

	case IDLN_TYPE_ARRAY:
		IDL_tree_walk_in_order(IDL_TYPE_ARRAY(p).ident, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_TYPE_ARRAY(p).size_list, tree_func, user_data);
		break;

	case IDLN_TYPE_STRUCT:
		IDL_tree_walk_in_order(IDL_TYPE_STRUCT(p).ident, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_TYPE_STRUCT(p).member_list, tree_func, user_data);
		break;
		
	case IDLN_TYPE_UNION:
		IDL_tree_walk_in_order(IDL_TYPE_UNION(p).ident, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_TYPE_UNION(p).switch_type_spec, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_TYPE_UNION(p).switch_body, tree_func, user_data);
		break;

	case IDLN_CASE_STMT:
		IDL_tree_walk_in_order(IDL_CASE_STMT(p).labels, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_CASE_STMT(p).element_spec, tree_func, user_data);
		break;

	case IDLN_INTERFACE:
		IDL_tree_walk_in_order(IDL_INTERFACE(p).ident, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_INTERFACE(p).inheritance_spec, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_INTERFACE(p).body, tree_func, user_data);
		break;

	case IDLN_MODULE:
		IDL_tree_walk_in_order(IDL_MODULE(p).ident, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_MODULE(p).definition_list, tree_func, user_data);
		break;		

	case IDLN_BINOP:
		IDL_tree_walk_in_order(IDL_BINOP(p).left, tree_func, user_data);
		IDL_tree_walk_in_order(IDL_BINOP(p).right, tree_func, user_data);
		break;

	case IDLN_UNARYOP:
		IDL_tree_walk_in_order(IDL_UNARYOP(p).operand, tree_func, user_data);
		break;
		
	default:
		g_warning("IDL_tree_walk_in_order: unknown node type %s\n", IDL_NODE_TYPE_NAME(p));
		break;
	}
}

static void __IDL_tree_free(IDL_tree p);

static int tree_free_but_this(IDL_tree data, IDL_tree p, IDL_tree this_one)
{
	if (p == this_one)
		return TRUE;

	__IDL_tree_free(p);

	return TRUE;
}

/* Free associated node data, regardless of refcounts */
static void IDL_tree_free_real(IDL_tree p)
{
	assert(p != NULL);

	switch (IDL_NODE_TYPE(p)) {
	case IDLN_GENTREE:
		g_hash_table_foreach(IDL_GENTREE(p).children, (GHFunc)tree_free_but_this, NULL);
		g_hash_table_destroy(IDL_GENTREE(p).children);
		break;

	case IDLN_FIXED:
		free(IDL_FIXED(p).value);
		break;

	case IDLN_STRING:
		free(IDL_STRING(p).value);
		break;

	case IDLN_CHAR:
		free(IDL_CHAR(p).value);
		break;

	case IDLN_IDENT:
		free(IDL_IDENT(p).str);
		free(IDL_IDENT_REPO_ID(p));
		break;

	default:
		break;
	}

	free(p);
}

/* Free node taking into account refcounts */
static void __IDL_tree_free(IDL_tree p)
{
	if (p == NULL)
		return;
	
	if (--IDL_NODE_REFS(p) <= 0)
		IDL_tree_free_real(p);
}

/* Free a set of references of an entire tree */
void IDL_tree_free(IDL_tree p)
{
	IDL_tree q;

	if (p == NULL)
		return;

	switch (IDL_NODE_TYPE(p)) {
	case IDLN_INTEGER:
	case IDLN_FLOAT:
	case IDLN_BOOLEAN:
	case IDLN_TYPE_FLOAT:		
	case IDLN_TYPE_INTEGER:
	case IDLN_TYPE_CHAR:
	case IDLN_TYPE_WIDE_CHAR:
	case IDLN_TYPE_BOOLEAN:
	case IDLN_TYPE_OCTET:
	case IDLN_TYPE_ANY:
	case IDLN_TYPE_OBJECT:
	case IDLN_FIXED:
	case IDLN_STRING:
	case IDLN_CHAR:
	case IDLN_IDENT:
		__IDL_tree_free(p);
		break;

	case IDLN_LIST:
		while (p) {
			IDL_tree_free(IDL_LIST(p).data);
			q = IDL_LIST(p).next;
			__IDL_tree_free(p);
			p = q;
		}
		break;

	case IDLN_GENTREE:
		g_hash_table_foreach(IDL_GENTREE(p).siblings, (GHFunc)tree_free_but_this, p);
		g_hash_table_destroy(IDL_GENTREE(p).siblings);
		__IDL_tree_free(p);
		break;

	case IDLN_MEMBER:
		IDL_tree_free(IDL_MEMBER(p).type_spec);
		IDL_tree_free(IDL_MEMBER(p).dcls);
		__IDL_tree_free(p);
		break;

	case IDLN_NATIVE:
		IDL_tree_free(IDL_NATIVE(p).ident);
		__IDL_tree_free(p);
		break;

	case IDLN_TYPE_ENUM:
		IDL_tree_free(IDL_TYPE_ENUM(p).ident);
		IDL_tree_free(IDL_TYPE_ENUM(p).enumerator_list);
		__IDL_tree_free(p);
		break;

	case IDLN_TYPE_SEQUENCE:
		IDL_tree_free(IDL_TYPE_SEQUENCE(p).simple_type_spec);
		IDL_tree_free(IDL_TYPE_SEQUENCE(p).positive_int_const);
		__IDL_tree_free(p);
		break;

	case IDLN_TYPE_ARRAY:
		IDL_tree_free(IDL_TYPE_ARRAY(p).ident);
		IDL_tree_free(IDL_TYPE_ARRAY(p).size_list);
		__IDL_tree_free(p);
		break;

	case IDLN_TYPE_STRUCT:
		IDL_tree_free(IDL_TYPE_STRUCT(p).ident);
		IDL_tree_free(IDL_TYPE_STRUCT(p).member_list);
		__IDL_tree_free(p);
		break;

	case IDLN_TYPE_UNION:
		IDL_tree_free(IDL_TYPE_UNION(p).ident);
		IDL_tree_free(IDL_TYPE_UNION(p).switch_type_spec);
		IDL_tree_free(IDL_TYPE_UNION(p).switch_body);
		__IDL_tree_free(p);
		break;
				
	case IDLN_TYPE_DCL:
		IDL_tree_free(IDL_TYPE_DCL(p).type_spec);
		IDL_tree_free(IDL_TYPE_DCL(p).dcls);
		__IDL_tree_free(p);
		break;

	case IDLN_CONST_DCL:
		IDL_tree_free(IDL_CONST_DCL(p).const_type);
		IDL_tree_free(IDL_CONST_DCL(p).ident);
		IDL_tree_free(IDL_CONST_DCL(p).const_exp);
		__IDL_tree_free(p);
		break;

	case IDLN_EXCEPT_DCL:
		IDL_tree_free(IDL_EXCEPT_DCL(p).ident);
		IDL_tree_free(IDL_EXCEPT_DCL(p).members);
		__IDL_tree_free(p);
		break;
		
	case IDLN_ATTR_DCL:
		IDL_tree_free(IDL_ATTR_DCL(p).param_type_spec);
		IDL_tree_free(IDL_ATTR_DCL(p).simple_declarations);
		__IDL_tree_free(p);
		break;
		
	case IDLN_OP_DCL:
		IDL_tree_free(IDL_OP_DCL(p).op_type_spec);
		IDL_tree_free(IDL_OP_DCL(p).ident);
		IDL_tree_free(IDL_OP_DCL(p).parameter_dcls);
		IDL_tree_free(IDL_OP_DCL(p).raises_expr);
		IDL_tree_free(IDL_OP_DCL(p).context_expr);
		__IDL_tree_free(p);
		break;

	case IDLN_PARAM_DCL:
		IDL_tree_free(IDL_PARAM_DCL(p).param_type_spec);
		IDL_tree_free(IDL_PARAM_DCL(p).simple_declarator);
		__IDL_tree_free(p);
		break;
		
	case IDLN_FORWARD_DCL:
		IDL_tree_free(IDL_FORWARD_DCL(p).ident);
		__IDL_tree_free(p);
		break;
		
	case IDLN_TYPE_STRING:
		IDL_tree_free(IDL_TYPE_STRING(p).positive_int_const);
		__IDL_tree_free(p);
		break;
		
	case IDLN_TYPE_WIDE_STRING:
		IDL_tree_free(IDL_TYPE_WIDE_STRING(p).positive_int_const);
		__IDL_tree_free(p);
		break;
		
	case IDLN_TYPE_FIXED:
		IDL_tree_free(IDL_TYPE_FIXED(p).positive_int_const);
		IDL_tree_free(IDL_TYPE_FIXED(p).integer_lit);
		__IDL_tree_free(p);
		break;

	case IDLN_CASE_STMT:
		IDL_tree_free(IDL_CASE_STMT(p).labels);
		IDL_tree_free(IDL_CASE_STMT(p).element_spec);
		__IDL_tree_free(p);
		break;
		
	case IDLN_INTERFACE:
		IDL_tree_free(IDL_INTERFACE(p).ident);
		IDL_tree_free(IDL_INTERFACE(p).inheritance_spec);
		IDL_tree_free(IDL_INTERFACE(p).body);
		__IDL_tree_free(p);
		break;

	case IDLN_MODULE:
		IDL_tree_free(IDL_MODULE(p).ident);
		IDL_tree_free(IDL_MODULE(p).definition_list);
		__IDL_tree_free(p);
		break;

	case IDLN_BINOP:
		IDL_tree_free(IDL_BINOP(p).left);
		IDL_tree_free(IDL_BINOP(p).right);
		__IDL_tree_free(p);
		break;

	case IDLN_UNARYOP:
		IDL_tree_free(IDL_UNARYOP(p).operand);
		__IDL_tree_free(p);
		break;		
		
	default:
		g_warning("Free unknown node: %d\n", IDL_NODE_TYPE(p));
		break;
	}
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
	IDL_tree curitem;
	int length;

	for(curitem = list, length = 0; curitem;
	    curitem = IDL_LIST(curitem).next)
		length++;

	return length;
}

IDL_tree IDL_list_nth(IDL_tree list, int n)
{
	IDL_tree curitem;
	int i;

	for(curitem = list, i = 0; i < n && curitem;
	    curitem = IDL_LIST(curitem).next, i++) ;

	return curitem;
}

struct remove_list_node_data {
	IDL_tree *root;
	GHashTable *removed_nodes;
};

static int remove_list_node(IDL_tree p, IDL_tree *list_head, struct remove_list_node_data *data)
{
	assert(p != NULL);
	assert(IDL_NODE_TYPE (p) == IDLN_LIST);

	if (list_head)
		*list_head = IDL_list_remove(*list_head, p);
	else
		*data->root = IDL_list_remove(*data->root, p);

	if (data->removed_nodes) {
		if (!g_hash_table_lookup_extended(data->removed_nodes, p, NULL, NULL))
			g_hash_table_insert(data->removed_nodes, p, p);
		/*
		  We shouldn't need this since we have removed it from the tree,
		  but we might need it for multiple declspec(inhibits) in the same
		  subtree.
		  IDL_tree_walk_in_order(p, (IDL_tree_func)inc_node_ref, NULL);
		*/
	} else
		IDL_tree_free(p);
	
	return TRUE;
}

/* Forward Declaration Resolution */
static int load_forward_dcls(IDL_tree p, GHashTable *table)
{
	if (IDL_NODE_TYPE(p) == IDLN_FORWARD_DCL) {
		char *s = IDL_ns_ident_to_qstring(IDL_FORWARD_DCL(p).ident, "::", 0);

		if (!g_hash_table_lookup_extended(table, s, NULL, NULL))
			g_hash_table_insert(table, s, p);
		else
			free(s);
	}

	return TRUE;
}

static int resolve_forward_dcls(IDL_tree p, GHashTable *table)
{
	if (IDL_NODE_TYPE(p) == IDLN_INTERFACE) {
		char *orig, *s = IDL_ns_ident_to_qstring(IDL_INTERFACE(p).ident, "::", 0);

		if (g_hash_table_lookup_extended(table, s, (gpointer)&orig, NULL)) {
			g_hash_table_remove(table, orig);
			free(orig);
		}
		free(s);
	}

	return TRUE;
}

static int print_unresolved_forward_dcls(char *s, IDL_tree p)
{
	yyerrornv(p, "Unresolved forward declaration `%s'", s);
	free(s);

	return TRUE;
}

static int load_forward_dcls_to_node_hash(char *s, IDL_tree p, GHashTable *node_hash)
{
	g_hash_table_insert(node_hash, IDL_NODE_UP(p), IDL_NODE_UP(IDL_NODE_UP(p)) ?
			    &IDL_MODULE(IDL_NODE_UP(IDL_NODE_UP(p))).definition_list : NULL);
	
	return TRUE;
}

void IDL_tree_process_forward_dcls(IDL_tree *p, IDL_ns ns)
{
	struct remove_list_node_data data;
	GHashTable *table = g_hash_table_new(IDL_strcase_hash, IDL_strcase_equal);
	GHashTable *node_hash = g_hash_table_new(g_direct_hash, g_direct_equal);

	IDL_tree_walk_in_order(*p, (IDL_tree_func)load_forward_dcls, table);
	IDL_tree_walk_in_order(*p, (IDL_tree_func)resolve_forward_dcls, table);
	g_hash_table_foreach(table, (GHFunc)print_unresolved_forward_dcls, NULL);
	g_hash_table_foreach(table, (GHFunc)load_forward_dcls_to_node_hash, node_hash);
	data.root = p;
	data.removed_nodes = NULL;
	g_hash_table_foreach(node_hash, (GHFunc)remove_list_node, &data);
	g_hash_table_destroy(node_hash);
	g_hash_table_destroy(table);
}

/* Inhibit Creation Removal */
static int load_inhibits(IDL_tree p, GHashTable *table)
{
	if ((IDL_NODE_TYPE(p) == IDLN_INTERFACE ||
	     IDL_NODE_TYPE(p) == IDLN_MODULE) &&
	    IDL_NODE_UP(p) &&
	    IDL_NODE_TYPE(IDL_NODE_UP(p)) == IDLN_LIST &&
	    IDL_NODE_DECLSPEC(p) & IDLF_DECLSPEC_INHIBIT &&
	    !g_hash_table_lookup_extended(table, IDL_NODE_UP(p), NULL, NULL)) {
		
		IDL_tree *list_head = NULL;
		
		if (IDL_NODE_UP(IDL_NODE_UP(p))) {
			assert(IDL_NODE_TYPE(IDL_NODE_UP(IDL_NODE_UP(p))) == IDLN_MODULE);
			list_head = &IDL_MODULE(IDL_NODE_UP(IDL_NODE_UP(p))).definition_list;
		}
		
		g_hash_table_insert(table, IDL_NODE_UP(p), list_head);
	}
	
	return TRUE;
}

void IDL_tree_remove_inhibits(IDL_tree *p, IDL_ns ns)
{
	struct remove_list_node_data data;
	GHashTable *table = g_hash_table_new(g_direct_hash, g_direct_equal);

	IDL_tree_walk_in_order(*p, (IDL_tree_func)load_inhibits, table);
	data.root = p;
	data.removed_nodes = IDL_NS(ns).inhibits;
	g_hash_table_foreach(table, (GHFunc)remove_list_node, &data);
	g_hash_table_destroy(table);
}

/* Multi-Pass Empty Module Removal */
static int load_empty_modules(IDL_tree p, GHashTable *table)
{
	if (IDL_NODE_TYPE(p) == IDLN_MODULE &&
	    IDL_MODULE(p).definition_list == NULL && 
	    IDL_NODE_UP(p) &&
	    IDL_NODE_TYPE(IDL_NODE_UP(p)) == IDLN_LIST &&
	    !g_hash_table_lookup_extended(table, IDL_NODE_UP(p), NULL, NULL)) {
		
		IDL_tree *list_head = NULL;
		
		if (IDL_NODE_UP(IDL_NODE_UP(p))) {
			assert(IDL_NODE_TYPE(IDL_NODE_UP(IDL_NODE_UP(p))) == IDLN_MODULE);
			list_head = &IDL_MODULE(IDL_NODE_UP(IDL_NODE_UP(p))).definition_list;
		}
		
		g_hash_table_insert(table, IDL_NODE_UP(p), list_head);
	}

	return TRUE;
}

void IDL_tree_remove_empty_modules(IDL_tree *p, IDL_ns ns)
{
	struct remove_list_node_data data;
	gboolean done = FALSE;

	data.root = p;
	data.removed_nodes = NULL;

	while (!done) {
		GHashTable *table = g_hash_table_new(g_direct_hash, g_direct_equal);
		IDL_tree_walk_in_order(*p, (IDL_tree_func)load_empty_modules, table);
		done = g_hash_table_size(table) == 0;
		g_hash_table_foreach(table, (GHFunc)remove_list_node, &data);
		g_hash_table_destroy(table);
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
