#include <string.h>
#include "util.h"

#ifndef HAVE_STRDUP
char *strdup(const char *s)
{
	char *p;
	if (!s) return NULL;
	p = (char *)malloc(strlen(s) + 1);
	strcpy(p, s);
	return p;
}
#endif
