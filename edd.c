#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ncurses.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>

#define ERROR(x) fprintf(stderr, "edd: %s", x);
#define MAXLINE 512

typedef struct line { // double linked list
	char *s;
	struct line *n;   // next
	struct line *p;   // prev
} line;

struct buf {
	line *first;
	line *cur;
	line *last;
	int line;         // display line on screen
	int linepos;      // position on line
};

void next_char(struct buf *b) {
	if(b->cur->s[b->linepos + 1] != '\0' && b->cur->s[b->linepos + 1] != '\n') {
		b->linepos++;
	}
}

void prev_char(struct buf *b) {
	if(b->linepos > 0) {
		b->linepos--;
	}
}

void prev_line(struct buf *b) {
	if(b->cur->p) {
		b->cur = b->cur->p;
		b->line--;
	}
}

void next_line(struct buf *b) {
	if(b->cur->n)
	{
		b->cur = b->cur->n;
		b->line++;
	}
}

void input(struct buf *b) {
	switch(getch())
	{
		case 'k':
			prev_line(b);
			break;
		case 'j':
			next_line(b);
			break;
		case 'l':
			next_char(b);
			break;
		case 'h':
			prev_char(b);
			break;
	}
}

// TODO: chunk file? if memory is ever an issue, that'll be the easiest thing to do.
line *load_file(struct buf *b, const char *fname) {
	FILE *f = fopen("./edd.c", "r");
	if(f == NULL) ERROR("invalid file");
	char s[MAXLINE];
	for(int i = 0; fgets(s, MAXLINE, f) != NULL; i++) {
		line *l = malloc(sizeof(line));
		l->s = strdup(s);
		if(i == 0) {
			l->n = NULL;
			l->p = NULL;
			b->first = l;
		} else {
			l->n = NULL;
			l->p = b->last;
			b->last->n = l;
		}
		b->last = l;
	}
}

void display(struct buf *b) {
	line *l = b->first;
	for(int i = 0; l != NULL; l = l->n, i++)
		mvaddstr(i, 0, l->s);
	move(b->line, b->linepos);
	refresh();
}

int main(void) {
	WINDOW *win;
	if((win = initscr()) == NULL) {fprintf(stderr, "error initializing ncurses");}
	noecho();

	struct buf b = {NULL, NULL, NULL, 0, 0};
	line *n = load_file(&b, "./edd.c");
	b.cur = b.first;

	while(1) {
		display(&b);
		input(&b);
	}

	endwin();
	delwin(win);
	refresh();
	return 0;
}
