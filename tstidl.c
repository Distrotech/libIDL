#include <stdio.h>
#include <idl.h>

int IDL_cb(int level, int num, int line, const char *filename, const char *s)
{
	fprintf(stderr, "%s:%d: %s\n", filename, line, s);
	return 1;
}

int main(int argc, char *argv[])
{
	int rv;
	IDL_tree tree, symtab;
	char *fn;

	if (argc != 2) {
		fprintf(stderr, "usage: tstidl <filename>\n");
		exit(1);
	}

	fn = argv[1];

	rv = IDL_parse_filename(fn, NULL, NULL, &tree, &symtab);

	if (rv == IDL_SUCCESS) {
		void __idl_print_tree(IDL_tree p);
		
		fprintf(stderr, "tstidl: IDL_SUCCESS: %p\n", tree);
		
		printf("Walking Root Tree\n");
		__idl_print_tree(tree);
		printf("Walking Symbol Table\n");
		__idl_print_tree(symtab);

		IDL_root_free(tree);
		IDL_symtab_free(symtab);
		
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
