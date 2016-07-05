#ifndef ASTR_H
#define ASTR_H

#include <stdlib.h>
#include <string.h>

typedef struct {			// auto growing string (only use when necessary)
	char *ss;			// content
	size_t s;			// size
	size_t l;			// length
} astr_t;

astr_t *newstr(char *content, size_t n);
void appendstr(astr_t *s, char *c);
void appendch(astr_t *s, char c);
void freestr(astr_t *s);
int remch(astr_t *s);

#endif // ASTR_H
