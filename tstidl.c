#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <idl.h>

void IDL_ns_print_idents(FILE *o, IDL_tree l)
{
	if (l == NULL)
		return;

	assert(IDL_NODE_TYPE(l) == IDLN_LIST);

	while (l != NULL) {
		assert(IDL_LIST(l).data != NULL);
		assert(IDL_NODE_TYPE(IDL_LIST(l).data) == IDLN_IDENT);
		fprintf(o, "::%s", IDL_IDENT(IDL_LIST(l).data).str);
		l = IDL_LIST(l).next;
	}
	fprintf(o, "\n");
}

void IDL_ns_rcs_traverse(IDL_tree p)
{
	if (p == NULL)
		return;

	assert(IDL_NODE_TYPE(p) == IDLN_GENTREE);

	if (IDL_GENTREE(p).children == NULL) {
		IDL_tree l = IDL_ns_qualified_ident_new(p);
		if (l != NULL) {
			IDL_ns_print_idents(stdout, l);
			IDL_tree_free(l);
		}
	}

	IDL_ns_rcs_traverse(IDL_GENTREE(p).children);
	IDL_ns_rcs_traverse(IDL_GENTREE(p).siblings);
}

void IDL_ns_dump_namespace(IDL_ns ns)
{
	IDL_ns_rcs_traverse(IDL_NS(ns).global);
}

int main(int argc, char *argv[])
{
	int rv;
	IDL_tree tree;
	IDL_ns ns;
	char *fn;
	extern int __IDL_debug;

	__IDL_debug = 0;

	if (argc != 3) {
		fprintf(stderr, "usage: tstidl <filename> <fold>\n");
		exit(1);
	}

	fn = argv[1];

	rv = IDL_parse_filename(fn, NULL, NULL, &tree, &ns, atoi(argv[2]));

	if (rv == IDL_SUCCESS) {
		void __IDL_tree_print(IDL_tree p);
		
		IDL_ns_dump_namespace(ns);
		IDL_tree_free(tree);
		IDL_ns_free(ns);
		
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
