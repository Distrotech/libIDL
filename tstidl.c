#ifdef G_LOG_DOMAIN
#  undef G_LOG_DOMAIN
#endif
#define G_LOG_DOMAIN		"tstidl"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <glib.h>
#ifdef IDL_LIBRARY
#  include "IDL.h"
#else
#  include <libIDL/IDL.h>
#endif

struct walk_data {
	IDL_tree tree;
	IDL_ns ns;
};

gboolean print_repo_id (IDL_tree p, IDL_tree parent, struct walk_data data)
{
	char *repo_id = NULL;

	if (IDL_NODE_TYPE (p) == IDLN_INTERFACE)
		repo_id = IDL_IDENT_REPO_ID (IDL_INTERFACE (p).ident);
	else if (IDL_NODE_TYPE (p) == IDLN_IDENT &&
		 IDL_NODE_UP (p) != NULL &&
		 IDL_NODE_UP (IDL_NODE_UP (p)) != NULL &&
		 IDL_NODE_TYPE (IDL_NODE_UP (IDL_NODE_UP (p))) == IDLN_ATTR_DCL)
		repo_id = IDL_IDENT_REPO_ID (p);

	if (repo_id)
		printf ("%s\n", repo_id);

	if (IDL_NODE_TYPE (p) == IDLN_INTERFACE) {
		const char *val;

		val = IDL_tree_property_get (p, "IID");
		if (val) printf ("\tXPIDL IID:\"%s\"\n", val);
	}


	if (IDL_NODE_TYPE (p) == IDLN_PARAM_DCL) {
		IDL_tree op = IDL_NODE_UP (IDL_NODE_UP (p));
		IDL_tree ident;
		IDL_tree q;		
		const char *val;

		assert (IDL_NODE_TYPE (op) == IDLN_OP_DCL);

		val = IDL_tree_property_get (p, "IID_IS");
		if (val) {
			printf ("\tXPIDL PARAM IID_IS: \"%s\"\n", val);

			ident = IDL_ident_new (g_strdup (val));

			if ((q = IDL_ns_lookup_this_scope (
				data.ns, IDL_IDENT_TO_NS (IDL_OP_DCL (op).ident),
				ident, NULL)) == NULL) {
				IDL_tree_error (op,
						"`%s' not found in parameter list",
						val);
			}

			IDL_tree_free (ident);
		}
	}
	
	if (IDL_NODE_TYPE (p) == IDLN_NATIVE) {
		if (IDL_NATIVE (p).user_type)
			g_message ("XPIDL native type: \"%s\"", IDL_NATIVE (p).user_type);
	}

	if (IDL_NODE_TYPE (p) == IDLN_CODEFRAG) {
		GSList *slist = IDL_CODEFRAG (p).lines;
		g_message ("XPIDL code fragment desc.: \"%s\"", IDL_CODEFRAG (p).desc);
		for (; slist; slist = slist->next)
			g_message ("XPIDL code fragment line.: \"%s\"", (char *) slist->data);
	}
	
	return TRUE;
}

gboolean print_ident_comments (IDL_tree p, IDL_tree parent, struct walk_data data)
{
	GSList *list;

	if (IDL_NODE_TYPE (p) == IDLN_IDENT) {
		printf ("Identifier: %s\n", IDL_IDENT (p).str);
		for (list = IDL_IDENT (p).comments; list;
		     list = g_slist_next (list)) {
			char *comment = list->data;
			printf ("%s\n", comment);
		}
	}

	return TRUE;
}

gboolean print_const_dcls (IDL_tree p, IDL_tree parent, struct walk_data data)
{
	if (IDL_NODE_TYPE (p) == IDLN_CONST_DCL &&
	    IDL_NODE_TYPE (IDL_CONST_DCL (p).const_exp) == IDLN_INTEGER) {
		printf ("%s is %" IDL_LL "d\n",
			IDL_IDENT (IDL_CONST_DCL (p).ident).str,
			IDL_INTEGER (IDL_CONST_DCL (p).const_exp).value);
		return FALSE;
	}
	return TRUE;
}

/* Example input method... simply reads in from some file and passes it along as
 * is--warning, no actual C preprocessing performed here!  Standard C preprocessor
 * indicators should also be passed to libIDL, including # <line> "filename" (double
 * quotes expected), etcetera (do not confuse this with passing #defines to libIDL, this
 * should not be done). */

/* #define TEST_INPUT_CB */

#ifdef _WIN32
#  ifndef TEST_INPUT_CB
#    define TEST_INPUT_CB
#  endif
#endif

struct my_input_cb_data {
	FILE *in;
};

int my_input_cb (IDL_input_reason reason, union IDL_input_data *cb_data, gpointer user_data)
{
	struct my_input_cb_data *my_data = user_data;
	int rv;
	
	switch (reason) {
	case IDL_INPUT_REASON_INIT:
		g_message ("my_input_cb: filename: %s", cb_data->init.filename);
		my_data->in = fopen (cb_data->init.filename, "r");
		/* If failed, should know that it is implied to libIDL that errno is set
		 * appropriately by a C library function or otherwise. Return 0 upon
		 * success. */
		return my_data->in ? 0 : -1;

	case IDL_INPUT_REASON_FILL:
		/* Fill the buffer here... return number of bytes read (maximum of
		   cb_data->fill.max_size), 0 for EOF, negative value upon error. */
		rv = fread (cb_data->fill.buffer, 1, cb_data->fill.max_size, my_data->in);
		IDL_queue_new_ident_comment ("Queue some comment...");
		g_message ("my_input_cb: fill, max size %d, got %d",
			   cb_data->fill.max_size, rv);
		if (rv == 0 && ferror (my_data->in))
			return -1;
		return rv;

	case IDL_INPUT_REASON_ABORT:
	case IDL_INPUT_REASON_FINISH:
		/* Called after parsing to indicate success or failure */
		g_message ("my_input_cb: abort or finish");
		fclose (my_data->in);
		break;
	}
	
	return 0;
}

int main (int argc, char *argv[])
{
	int rv;
	IDL_tree tree;
	IDL_ns ns;
	char *fn;
	struct walk_data data;
	unsigned long parse_flags = 0;

#ifndef _WIN32
	{ extern int __IDL_debug;
	__IDL_debug = argc >= 4 ? TRUE : FALSE; }
#endif
	
	IDL_check_cast_enable (TRUE);

	if (argc < 2) {
		fprintf (stderr, "usage: tstidl <filename> [parse flags, hex]\n");
		exit (1);
	}

	fn = argv[1];
	if (argc >= 3)
		sscanf (argv[2], "%lx", &parse_flags);
	
#ifdef TEST_INPUT_CB
	{ struct my_input_cb_data input_cb_data;
	g_message ("IDL_parse_filename_with_input");
	rv = IDL_parse_filename_with_input (
		fn, my_input_cb, &input_cb_data,
		NULL, &tree, &ns, parse_flags,
		IDL_WARNING1);
	}
#else
	g_message ("IDL_parse_filename");
	rv = IDL_parse_filename (
		fn, NULL, NULL, &tree, &ns,
		parse_flags, IDL_WARNING1);
#endif

	if (rv == IDL_ERROR) {
		g_message ("IDL_ERROR");
		exit (1);	
	} else if (rv < 0) {
		perror (fn);
		exit (1);
	}

	/* rv == IDL_SUCCESS */

	data.tree = tree;
	data.ns = ns;
	
	printf ("Repository IDs\n");
	IDL_tree_walk_in_order (tree, (IDL_tree_func) print_repo_id, &data);
	printf ("\nConstant Declarations\n");
	IDL_tree_walk_in_order (tree, (IDL_tree_func) print_const_dcls, &data);
	printf ("\nIdentifiers\n");
	IDL_tree_walk_in_order (tree, (IDL_tree_func) print_ident_comments, &data);
	printf ("\nIDL tree to IDL\n");
	IDL_tree_to_IDL (tree, ns, stdout, parse_flags >> 24);
	IDL_ns_free (ns);
	IDL_tree_free (tree);
	
	return 0;
}
