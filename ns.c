/***************************************************************************

    ns.c (libIDL namespace functions)

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

static int my_strcmp(const char *a, const char *b)
{
	int rv = strcasecmp(a, b);
	
	if (__IDL_is_parsing && rv == 0 && strcmp(a, b) != 0) {
		yywarningv(IDL_WARNING1, "Case mismatch between `%s' and `%s' ", a, b);
		yywarning(IDL_WARNING1, "(Identifiers should be case-consistent after initial declaration)");
	}

	return rv;
}

static int identcmp(IDL_tree a, IDL_tree b)
{
	assert(IDL_NODE_TYPE(a) == IDLN_IDENT);
	assert(IDL_NODE_TYPE(b) == IDLN_IDENT);
	return my_strcmp(IDL_IDENT(a).str, IDL_IDENT(b).str);
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

	IDL_tree_free(IDL_NS(ns).global);

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
	IDL_IDENT_REPO_ID(ident) = IDL_ns_ident_make_repo_id(__IDL_root_ns, p, NULL, NULL, NULL);

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
		IDL_tree_get_node_info(q, &what1, &who1);

		q = any;
		while (q && (IDL_NODE_TYPE(q) == IDLN_IDENT || IDL_NODE_TYPE(q) == IDLN_LIST))
			q = IDL_NODE_UP(q);
		assert(q != NULL);
		IDL_tree_get_node_info(q, &what2, &who2);

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
		
		if (!(r = IDL_ns_load_idents_to_tables(interface_ident, IDL_LIST(q).data,
						       ident_heap, visited_interfaces)))
			insert_conflict = 1;
	}
	
	mark_visited_interface(visited_interfaces, scope);

	return insert_conflict == 0;
}

int IDL_ns_check_for_ambiguous_inheritance(IDL_tree interface_ident, IDL_tree p)
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
