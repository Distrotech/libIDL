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

	rv = IDL_parse_filename (fn, NULL, NULL, &tree, &ns,
				 argc == 3 ? atoi (argv[2]) : 0, IDL_WARNING1);

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
