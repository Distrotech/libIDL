#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <glib.h>
#include "util.h"

gboolean print_repo_id (IDL_tree p, gpointer user_data)
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

	return TRUE;
}

gboolean print_const_dcls (IDL_tree p, gpointer user_data)
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
	extern int __IDL_debug;

	IDL_check_cast_enable (TRUE);
	__IDL_debug = FALSE;

	if (argc < 2) {
		fprintf (stderr, "usage: tstidl <filename> [inhibit constant folding (0 or 1)]\n");
		exit (1);
	}

	fn = argv[1];
	
#ifdef TEST_INPUT_CB
	{ struct my_input_cb_data input_cb_data;
	g_message ("IDL_parse_filename_with_input");
	rv = IDL_parse_filename_with_input (fn, my_input_cb, &input_cb_data,
					    NULL, &tree, &ns,
					    argc == 3 ? atoi (argv[2]) : 0, IDL_WARNING1);
	}
#else
	g_message ("IDL_parse_filename");
	rv = IDL_parse_filename (fn, NULL, NULL, &tree, &ns,
				 argc == 3 ? atoi (argv[2]) : 0, IDL_WARNING1);
#endif

	if (rv == IDL_SUCCESS) {
		printf ("Repository IDs\n");
		IDL_tree_walk_in_order (tree, print_repo_id, NULL);
		printf ("\nConstant Declarations\n");
		IDL_tree_walk_in_order (tree, print_const_dcls, NULL);
		IDL_ns_free (ns);
		IDL_tree_free (tree);
	}
	else if (rv == IDL_ERROR) {
		fprintf (stderr, "tstidl: IDL_ERROR\n");
		exit (1);	
	}
	else if (rv < 0) {
		perror (fn);
		exit (1);
	}
	
	return 0;
}
