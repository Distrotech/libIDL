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
	char *repo_id = NULL;

	if (IDL_NODE_TYPE(p) == IDLN_INTERFACE)
		repo_id = IDL_IDENT_REPO_ID(IDL_INTERFACE(p).ident);
	else if (IDL_NODE_TYPE(p) == IDLN_IDENT &&
		 IDL_NODE_UP(p) != NULL &&
		 IDL_NODE_UP(IDL_NODE_UP(p)) != NULL &&
		 IDL_NODE_TYPE(IDL_NODE_UP(IDL_NODE_UP(p))) == IDLN_ATTR_DCL)
		repo_id = IDL_IDENT_REPO_ID(p);

	if (repo_id)
		printf("%s\n", repo_id);

	return IDL_TRUE;
}

int print_interface_forwards(IDL_tree p, gpointer user_data)
{
	if (IDL_NODE_TYPE(p) == IDLN_INTERFACE) {
		char *s = IDL_ns_ident_to_qstring(IDL_INTERFACE(p).ident, "_", 0);
		
		printf("struct %s;\n", s);

		free(s);
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
	__IDL_debug = IDL_FALSE;

	if (argc < 2) {
		fprintf(stderr, "usage: tstidl <filename> [fold constants, 0 or 1]\n");
		exit(1);
	}

	fn = argv[1];

	rv = IDL_parse_filename(fn, NULL, NULL, &tree, &ns, argc == 3 ? atoi(argv[2]) : 0);

	if (rv == IDL_SUCCESS) {
		IDL_tree_walk_pre_order(tree, print_interface_forwards, NULL);
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
