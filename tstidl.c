#include <stdio.h>
#include <idl.h>

int main(int argc, char *argv[])
{
	int rv;
	IDL_tree tree;
	char *fn;

	if (argc != 2) {
		fprintf(stderr, "usage: tstidl <filename>\n");
		exit(1);
	}

	fn = argv[1];

	rv = IDL_parse_filename(fn, NULL, &tree);

	if (rv == IDL_SUCCESS) {
		
		fprintf(stderr, "tstidl: IDL_SUCCESS\n");
		
	}
	else if (rv == IDL_ERROR) {
		fprintf(stderr, "tstidl: IDL_ERROR: %s\n",
			IDL_get_last_error());
		exit(1);	
	}
	else if (rv < 0) {
		perror(fn);
		exit(1);
	}
	
	return 0;
}
