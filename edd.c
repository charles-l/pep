#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ncurses.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <ctype.h>

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

int eol_char(char c) {
	return c == '\0' || c == '\n';
}

char get_c(struct buf *b) {
	return b->cur->s[b->linepos];
}

char get_nc(struct buf *b) {
	if(get_c(b) == '\0') return '\0';
	return b->cur->s[b->linepos];
}

char get_pc(struct buf *b) {
	if(b->linepos == 0) return get_c(b);
	return b->cur->s[b->linepos - 1];
}

int m_next_char(struct buf *b) {
	if(!eol_char(b->cur->s[b->linepos + 1])) {
		b->linepos++;
		return 1;
	}
	return 0;
}

int m_next_word(struct buf *b) {
	if(m_next_char(b))
		if(!isalpha(get_c(b)) && !isspace(get_c(b)))
			return 1;
		else
			return m_next_word(b);
	else {
		m_next_line(b);
		return m_bol(b);
	}
	return 1; // shouldn't get here
}

int m_prev_char(struct buf *b) {
	if(b->linepos > 0) {
		b->linepos--;
		return 1;
	}
	return 0;
}

int m_eol(struct buf *b) {
	for(b->linepos = 0; !eol_char(b->cur->s[b->linepos + 1]); b->linepos++);
	return 1;
}

int m_bol(struct buf *b) {
	b->linepos = 0;
	return 1;
}

int m_prev_line(struct buf *b) {
	if(b->cur->p) {
		int oldpos = b->linepos;
		b->cur = b->cur->p;
		b->line--;
		if(oldpos > ((int) strlen(b->cur->s) - 2))
			m_eol(b);
		return 1;
	}
	return 0;
}

int m_next_line(struct buf *b) {
	if(b->cur->n)
	{
		int oldpos = b->linepos;
		b->cur = b->cur->n;
		b->line++;
		if(oldpos > ((int) strlen(b->cur->s) - 2))
			m_eol(b);
		return 1;
	}
	return 0;
}

void input(struct buf *b) {
	switch(getch())
	{
		case 'k':
			m_prev_line(b);
			break;
		case 'j':
			m_next_line(b);
			break;
		case 'l':
			m_next_char(b);
			break;
		case 'h':
			m_prev_char(b);
			break;
		case 'w':
			m_next_word(b);
			break;
		case '$':
			m_eol(b);
			break;
		case '^':
			m_bol(b);
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
	{
		if(i > LINES - 1) break; // TODO: don't use LINES to allow for rescaling
		move(i, 0);
		for(char *c = l->s; c[0] != '\0'; c++) {
			switch(c[0]) {
				case '\t':
					addstr(" "); // TODO: fixme
					break;
				default:
					addch(c[0]);
					break;
			}
		}
	}
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
