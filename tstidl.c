#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <glib.h>
#include "util.h"
#include "IDL.h"

int print_repo_id(IDL_tree p, gpointer user_data)
{
	if (IDL_NODE_TYPE(p) == IDLN_IDENT && IDL_IDENT_REPO_ID(p)) {
		printf("%s\n", IDL_IDENT_REPO_ID(p));
	}
	return IDL_TRUE;
}

int main(int argc, char *argv[])
{
	int rv;
	IDL_tree tree;
	IDL_ns ns;
	char *fn;
	extern int __IDL_debug;

	IDL_check_cast_enable(IDL_TRUE);
	__IDL_debug = 0;

	if (argc < 2) {
		fprintf(stderr, "usage: tstidl <filename> [fold constants, 0 or 1]\n");
		exit(1);
	}

	fn = argv[1];

	rv = IDL_parse_filename(fn, NULL, NULL, &tree, &ns, argc == 3 ? atoi(argv[2]) : 0);

	if (rv == IDL_SUCCESS) {
		IDL_tree_walk_pre_order(tree, print_repo_id, NULL);
		IDL_ns_free(ns);
		IDL_tree_free(tree);
	}
	else if (rv == IDL_ERROR) {
		fprintf(stderr, "tstidl: IDL_ERROR\n");
		exit(1);	
	}
	else if (rv < 0) {
		perror(fn);
		exit(1);
	}
	
	return 0;
}
