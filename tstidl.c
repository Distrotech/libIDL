#include <stdio.h>
#include <stdlib.h>
#include <idl.h>

int IDL_cb(int level, int num, int line, const char *filename, const char *s)
{
	fprintf(stderr, "%s:%d: %s\n", filename, line, s);
	return 1;
}

int main(int argc, char *argv[])
{
	int rv;
	IDL_tree tree;
	IDL_ns ns;
	char *fn;
	extern int __IDL_debug;

	__IDL_debug = 1;

	if (argc != 3) {
		fprintf(stderr, "usage: tstidl <filename> <fold>\n");
		exit(1);
	}

	fn = argv[1];

	rv = IDL_parse_filename(fn, NULL, NULL, &tree, &ns, atoi(argv[2]));

	if (rv == IDL_SUCCESS) {
		void __IDL_tree_print(IDL_tree p);
		
#if 1
		fprintf(stderr, "tstidl: IDL_SUCCESS: %p\n", tree);		
		printf("Walking Root Tree\n");
		__IDL_tree_print(tree);
		printf("Walking Symbol Table\n");
		__IDL_tree_print(IDL_NS(ns).global);
#else
		emit_CXX(tree);
#endif

#if 0
		IDL_tree_free(tree);
#endif
#if 0
		IDL_ns_free(ns);
#endif
		
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
