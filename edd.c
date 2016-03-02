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

#define KEY_ESC 27

typedef struct line { // double linked list
	char *s;
	struct line *n;   // next
	struct line *p;   // prev
} line;

typedef struct {
	line *l; // line
	int p;   // linepos
} pos;

struct buf {
	line *first;
	line *cur;
	line *last;
	line *scroll;     // top of current scroll position
	int line;         // display line on screen
	int linepos;      // position on line
};

int eol_char(char c);
char get_c(struct buf *b);
char get_nc(struct buf *b);
char get_pc(struct buf *b);
int m_next_word(struct buf *b);   		// motions just move the cursor
int m_prev_word(struct buf *b);			// motions *must* return the driection they went
int m_next_char(struct buf *b);
int m_prev_char(struct buf *b);
int m_next_line(struct buf *b);
int m_prev_line(struct buf *b);
int m_eol(struct buf *b);
int m_bol(struct buf *b);
int e_delete_line(struct buf *b);       // edits use track motions and operate in the in-between text
int e_insert(struct buf *b);
void input(struct buf *b);
line *load_file(struct buf *b, const char *fname);
void display(struct buf *b);

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
	return 1;
}

int m_prev_word(struct buf *b) {
	if(m_prev_char(b))
		if(!isalpha(get_c(b)) && !isspace(get_c(b)))
			return -1;
		else
			return m_prev_word(b);
	else {
		m_prev_line(b);
		return m_eol(b);
	}
	return -1;
}

int m_prev_char(struct buf *b) {
	if(b->linepos > 0) {
		b->linepos--;
		return -1;
	}
	return -1;
}

int m_eol(struct buf *b) {
	for(b->linepos = 0; !eol_char(b->cur->s[b->linepos + 1]); b->linepos++);
	return 1;
}

int m_bol(struct buf *b) {
	b->linepos = 0;
	return -1;
}

int m_prev_line(struct buf *b) {
	if(b->cur->p) {
		int oldpos = b->linepos;
		b->cur = b->cur->p;
		if(b->line == 0)
			b->scroll = b->scroll->p;
		else
			b->line--;
		if(oldpos > ((int) strlen(b->cur->s) - 2))
			m_eol(b);
		return -1;
	}
	return 0;
}

int m_next_line(struct buf *b) {
	if(b->cur->n) {
		int oldpos = b->linepos;
		b->cur = b->cur->n;
		if(b->line >= LINES - 1)
			b->scroll = b->scroll->n;
		else
			b->line++;
		if(oldpos > ((int) strlen(b->cur->s) - 2))
			m_eol(b);
		return 1;
	}
	return 0;
}

#define START (start.l->s + start.p)
#define END	  (end.l->s + end.p)

// TODO: support line deletes
int e_delete(struct buf *b, pos start, pos end) {
	memmove(START, END, strlen(END) + 1);
	return 0;
}

char *insert_str(struct buf *b) {
	char *r = malloc(256); // FIXME: could overflow
	r[0] = '\0';
	int i = 1;
	char c;
	while((c = getch()) != KEY_ESC) {
		switch(c) {
			default:
				r[i - 1] = c;
				r[i++] = '\0';
				move(b->line, 0);
				addnstr(b->cur->s, b->linepos);
				addstr(r);
				addstr(b->cur->s + b->linepos);
				move(b->line, b->linepos + i - 1);
				refresh();
				break;
		}
	}
	return r;
}

int e_insert(struct buf *b) { // TODO: refactor
	char *i = insert_str(b);
	size_t l = strlen(b->cur->s) + strlen(i);
	char *n = malloc(l);
	strncpy(n, b->cur->s, b->linepos);
	n[b->linepos] = '\0';
	strcat(n, i);
	strcat(n, b->cur->s + b->linepos);
	free(b->cur->s);
	b->cur->s = n;
	b->linepos = b->linepos + strlen(i) - 1;
	return 0;
}

int do_motion (struct buf *b, char c) { // returns direction of motion
	switch(c) {
		case 'k':
			return m_prev_line(b);
		case 'j':
			return m_next_line(b);
		case 'l':
			return m_next_char(b);
		case 'h':
			return m_prev_char(b);
		case 'w':
			return m_next_word(b);
		case 'b':
			return m_prev_word(b);
		case '$':
			return m_eol(b);
		case '^':
			return m_bol(b);
		default:
			return 0;
	}
}

pos get_pos(struct buf *b) {
	pos p = {b->cur, b->linepos};
	return p;
}

int swap(pos *s, pos *e) {
	pos t;
	t = *e;
	*e = *s;
	*s = t;
	return 1;
}

#define EDIT_MOTION(edit, motion) s = get_pos(b);        \
								  d = motion;	         \
								  e = get_pos(b);        \
							      if(d<0) swap(&s, &e);  \
								  edit(b, s, e);         \
								  b->linepos = s.p;

void input(struct buf *b) {
	char c; // character input
	pos s;  // start
	pos e;  // end
	int d;  // direction
	switch(c = getch()) {
		case 'd':
			EDIT_MOTION(e_delete, do_motion(b, getch()));
			clrtoeol();
			break;
		case 'i':
			e_insert(b);
			break;
		case 'x':
			EDIT_MOTION(e_delete, m_next_char(b));
			clrtoeol();
			break;
		default:
			if(do_motion(b, c))
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
	line *l = b->scroll;
	for(int i = 0; l != NULL; l = l->n, i++) {
		if(i > LINES - 1) break; // TODO: don't use LINES to allow for rescaling
		move(i, 0);
		for(char *c = l->s; c[0] != '\0'; c++) {
			switch(c[0]) {
				case '\t':
					addstr(" "); // TODO: fixme
					break;
				case '\n':
					addstr(".");
					break;
				case '\0':
					addstr("0");
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
	b.scroll = b.first;

	while(1) {
		display(&b);
		input(&b);
	}

	endwin();
	delwin(win);
	refresh();
	return 0;
}
