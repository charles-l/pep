#include "astr.h"
//// AUTO GROWING STRINGS ////

astr_t *newstr(char *content, size_t n) {
	astr_t *s = malloc(sizeof(astr_t));
	s->l = strlen(content);
	s->s = n;
	s->ss = malloc(s->s);
	strcpy(s->ss, content);
	return s;
}

void appendstr(astr_t *s, char *c) {
	size_t l = strlen(c);
	if(s->l + l + 1 > s->s) {
		s->s *= 2;
		s->ss = realloc(s->ss, s->s);
	}
	s->l += l;
	strcat(s->ss, c);
}

void appendch(astr_t *s, char c) {
	if(s->l + 1 > s->s) {
		s->s *= 2;
		s->ss = realloc(s->ss, s->s);
	}
	s->ss[s->l++] = c;
	s->ss[s->l] = '\0';
}

int remch(astr_t *s) { // backspace
	if(s->l > 0) {
		s->ss[--s->l] = '\0';
		return 1;
	}
	return 0;
}

void freestr(astr_t *s) {
	free(s->ss);
	free(s);
	s = NULL;
}
