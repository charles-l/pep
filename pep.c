#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ncurses.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <ctype.h>

// configs
#define MAXLINE 1024  			// maximum possible line length
#define TABSTOP 8     			// width of tab
#define STATUS_LENGTH 256		// max length of status text
#define COMMAND_LEN 256 		// max command length

#define ERROR(x) fprintf(stderr, "pep: %s", x);
#define KEY_ESC 0x1B    		// escape keycode
#define KEY_BS 0x7F    			// backspace keycode

typedef struct line { 			// double linked list
	char *s;
	struct line *n;   		// next
	struct line *p;   		// prev
} line;

typedef struct {
	line *l; 		  	// line
	int p;   		  	// linepos
} pos;

typedef struct undo { 			// saves an entire line in the undo list
	pos p;			  	// undo position
	line *l;			// stores changes
	enum {DELETED, CHANGED} t;	// whether line was deleted or changed
	struct undo *n;	  		// next undo (linked list)
} undo;

typedef struct string {			// auto growing string (only use when necessary)
	char *ss;			// content
	size_t s;			// size
	size_t l;			// length
} string;				// TODO: perhaps throw into its own file?

typedef struct {
	line *first;	  		// first line in file
	line *cur;		  	// current line
	line *last;       		// last line in file
	line *scroll;     		// top of current scroll position
	int linepos;      		// byte position on line
	int getvlnpos;     		// visual line position (for tabs, utf8)
	undo *undos;  			// linked list of undos
	undo *redos;  			// linked list of redos
	char *filename;			// buffers filename
} buf;

int is_eolch(char c);
pos curpos(buf *b);
int delln(buf *b, line *l);
char curch(buf *b);			// current character
char nextch(buf *b);			// next character
char prevch(buf *b);			// previous character
int eos(char *c);			// distance to end of string
int getcurln(buf *b);			// get visual y position of cursor
int getvlnpos(buf *b);			// get visual x cursor position
int swap(pos *s, pos *e);
line *rngcpy(pos *start, pos *end);
line *lncpy(pos *start, pos *end);
void pushundo(buf *b, pos *start, pos *end);
string *newstr(char *content, size_t n);
void appendstr(string *s, char *c);
void appendch(string *s, char c);
void freestr(string *s);

int m_nextch(buf *b);
int m_nextwrd(buf *b);
int m_prevwrd(buf *b);
int m_prevch(buf *b);
int m_eol(buf *b);
int m_bol(buf *b);
int m_jump(buf *b, pos start);
int m_prevln(buf *b);
int m_nextln(buf *b);

int e_join(buf *b, pos start, pos end);
int e_del(buf *b, pos start, pos end);
int e_insert(buf *b);
int e_new_line(buf *b);
int e_undo(buf *b, pos start, pos end);

line *loadfilebuf(buf *b, const char *fname);
void freebuf(buf *b);
void drawbuf(buf *b);
void filestatus(buf *b);

void mvmsg();
void showmsg(char *s);
void promptcmd(buf *b);

void command_mode(buf *b);
char *insert_mode(buf *b);
void quit(buf *b);

// globals
WINDOW *win;

int is_eolch(char c) {
	return c == '\0' || c == '\n';
}

pos curpos(buf *b) {
	pos p = {b->cur, b->linepos};
	return p;
}

line *dupln(line *l) {
	line *r = malloc(sizeof(line));
	*r = *l;
	return r;
}

int delln(buf *b, line *l) {
	if(l->p) l->p->n = l->n ? l->n : NULL;
	if(l->n) l->n->p = l->p ? l->p : NULL;

	if(l == b->scroll) b->scroll = b->scroll->n;
	if(l == b->cur)    b->cur = b->cur->n;

	free(l->s);
	free(l); l = NULL;
	clrtobot();
}

char curch(buf *b) {
	return b->cur->s[b->linepos];
}

char nextch(buf *b) {
	if(curch(b) == '\0') return '\0';
	return b->cur->s[b->linepos];
}

char prevch(buf *b) {
	if(b->linepos == 0) return curch(b);
	return b->cur->s[b->linepos - 1];
}

int eos(char *c) {
	int i = 0;
	while(c[0] != '\0') {c++; i++;}
	return i;
}

int getcurln(buf *b) {
	int i = 0;
	for(line *l = b->scroll; l != b->cur; l = l->n, i++);
	return i;
}

int getvlnpos(buf *b) {
	int i = 0;
	for(int j = 0; j < b->linepos; j++)
		switch(b->cur->s[j]) {
			case '\t':
				i += TABSTOP;
				break;
			default:
				i++;
				break;
		}
	return i;
}

int swap(pos *s, pos *e) {
	pos t;
	t = *e;
	*e = *s;
	*s = t;
	return 0;
}

line *rngcpy(pos *start, pos *end) { // TODO: remove or combine with lncpy
	line *r = malloc(sizeof(line));
	r->n = NULL;
	r->p = NULL;
	if(start->l == end->l) {
		r->s = malloc(end->p - start->p);
		memcpy(r->s, start->l + start->p, end->p - start->p);
	} // TODO: handle multiline ranges
	return r;
}

line *lncpy(pos *start, pos *end) { // TODO: remove or combine with rngcpy
	line *r = malloc(sizeof(line));
	r->n = NULL;
	r->p = NULL;
	if(start->l == end->l) {
		r->s = malloc(strlen(start->l->s));
		strcpy(r->s, start->l->s);
	}
	return r;
}

void pushundo(buf *b, pos *start, pos *end) {
	undo *u = malloc(sizeof(undo));
	u->p = curpos(b);
	u->n = b->undos;
	if(start->l == end->l)
		u->l = lncpy(start, end);
	else
		u->l = rngcpy(start, end);
	b->undos = u;
}

//// AUTO GROWING STRINGS ////

string *newstr(char *content, size_t n) {
	string *s = malloc(sizeof(string));
	s->l = strlen(content);
	s->s = n;
	s->ss = malloc(s->s);
	strcpy(s->ss, content);
	return s;
}

void appendstr(string *s, char *c) {
	size_t l = strlen(c);
	if(s->l + l + 1 > s->s) {
		s->s *= 2;
		s->ss = realloc(s->ss, s->s);
	}
	s->l += l;
	strcat(s->ss, c);
}

void appendch(string *s, char c) {
	if(s->l + 1 > s->s) {
		s->s *= 2;
		s->ss = realloc(s->ss, s->s);
	}
	s->ss[s->l++] = c;
	s->ss[s->l] = '\0';
}

void remch(string *s) { // backspace
	if(s->l > 0)
		s->ss[--s->l] = '\0';
}

void freestr(string *s) {
	free(s->ss);
	free(s);
	s = NULL;
}

//// MOTIONS ////

int m_nextch(buf *b) {
	if(!is_eolch(nextch(b))) {
		b->linepos++;
		return 1;
	}
	return 0;
}

int m_nextwrd(buf *b) {
	if(m_nextch(b))
		if(!isalpha(curch(b)) && !isspace(curch(b)))
			return 1;
		else
			return m_nextwrd(b);
	else {
		m_nextln(b);
		return m_bol(b);
	}
	return 1;
}

int m_prevwrd(buf *b) {
	if(m_prevch(b))
		if(!isalpha(curch(b)) && !isspace(curch(b)))
			return -1;
		else
			return m_prevwrd(b);
	else {
		m_prevln(b);
		return m_eol(b);
	}
	return -1;
}

int m_prevch(buf *b) {
	if(b->linepos > 0) {
		b->linepos--;
		return -1;
	}
	return 0;
}

int m_eol(buf *b) {
	for(b->linepos = 0; !is_eolch(nextch(b)); b->linepos++);
	return 1;
}

int m_bol(buf *b) {
	b->linepos = 0;
	return -1;
}

int m_jump(buf *b, pos start) { // ignores end
	b->cur = start.l;
	b->scroll = start.l;
	clear();
}

int m_prevln(buf *b) {
	if(b->cur->p) {
		int oldpos = b->linepos;
		b->cur = b->cur->p;
		if(b->cur == b->scroll && b->scroll->p) {
			b->scroll = b->scroll->p;
			scrl(-1); // this is a junk call to make sure
		}			  // scrolling doesn't goof display
		if(oldpos > ((int) strlen(b->cur->s) - 2))
			m_eol(b);
		return -1;
	}
	return 0;
}

int m_nextln(buf *b) {
	if(b->cur->n) {
		int oldpos = b->linepos;
		b->cur = b->cur->n;
		if(getcurln(b) >= LINES - 2) {
			b->scroll = b->scroll->n;
			scrl(1); // so is this one
		}
		if(oldpos > ((int) strlen(b->cur->s) - 2))
			m_eol(b);
		return 1;
	}
	return 0;
}

//// EDITS ////

#define STARTC (start.l->s + start.p)
#define ENDC   (end.l->s + end.p)

int e_join(buf *b, pos start, pos end) {
	size_t i = strlen(b->cur->s) + strlen(b->cur->n->s);
	char e;
	char *s = malloc(i + 1);
	strcat(s, b->cur->s);

	e = eos(s) - 1;	// remove newline
	if(s[e] == '\n') s[e] = '\0';

	strcat(s, b->cur->n->s);
	delln(b, b->cur->n);
	free(b->cur->s);
	b->cur->s = s;
	return 0;
}

int e_del(buf *b, pos start, pos end) {
	pushundo(b, &start, &end);
	if(start.l == end.l)
		memmove(STARTC, ENDC, strlen(ENDC) + 1); // FIXME?
	else {
		// for visual selection:
		//int i = eos(start.l->s);
		//e_del(b, start, (pos) {start.l, i});
		for(line *l = start.l; l != end.l->n; l = l->n) delln(b, l);
	}
	return 0;
}

char *insrtstr(char *s, char *i, int p) {
	char *r = malloc(strlen(s) + strlen(i));
	strncpy(r, s, p);
	r[p] = '\0';
	strcat(r, i);
	strcat(r, s + p);
	return r;
}

int e_insert(buf *b) { // TODO: refactor
	char *i = insert_mode(b);
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

int e_new_line(buf *b) {
	line *l = malloc(sizeof(line));
	l->n = b->cur->n;
	l->p = b->cur;
	l->s = strdup("");
	b->cur->n->p = l;
	b->cur->n = l;
	b->cur = l;
	clrtobot();
	return 0;
}

int e_undo(buf *b, pos start, pos end) {
	if(b->undos != NULL) {
		b->undos->l->n = b->undos->p.l->n;
		b->undos->l->p = b->undos->p.l->p;

		*(b->undos->p.l) = *(b->undos->l); // TODO: maybe memcpy?

		b->cur = b->undos->p.l;
		b->linepos = b->undos->p.p;

		undo *u = b->undos;
		b->undos = b->undos->n;
		free(u);
	}
	return 0;
}

//// BUFFER FUNCTIONS ////

// TODO: chunk file? if memory is ever an issue,
// that'll be the easiest thing to do.
line *loadfilebuf(buf *b, const char *fname) {
	FILE *f = fopen(fname, "r");
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
	b->filename = (char *) fname;
}

void freebuf(buf *b) {
	while(b->first != NULL) {
		free(b->first->s);
		free(b->first);
		b->first = b->first->n;
	}
}

void drawbuf(buf *b) {
	line *l = b->scroll;
	b->getvlnpos = getvlnpos(b);
	for(int i = 0; l != NULL; l = l->n, i++) {
		if(i > LINES - 2) break;
		move(i, 0);
		for(char *c = l->s; c[0] != '\0'; c++) {
			switch(c[0]) {
				case '\t':
					for(int i = 0; i < TABSTOP; i++)
						addch(' ');
					break;
				default:
					addch(c[0]);
					break;
			}
		}
	}
	move(getcurln(b), b->getvlnpos);
	refresh();
}

//// PROMPT ////

void filestatus(buf *b) {
	char s[STATUS_LENGTH];
	strncat(s, "editing ", STATUS_LENGTH);
	strncat(s, b->filename, STATUS_LENGTH);
	showmsg(s);
}

void mvmsg() { // move to the message box
	move(LINES - 1, 0);
}

void showmsg(char *s) { // display a message in the prompt box
	mvmsg();
	clrtoeol();
	addstr(s);
	wredrawln(win, LINES - 1, 1);
}

void promptcmd(buf *b) { // TODO: refactor
	char com[COMMAND_LEN];

	showmsg(":"); // pop prompt

	echo();
	scrollok(win, 0);
	getnstr(com, COMMAND_LEN);
	scrollok(win, 1);
	noecho();

	if(com[0] == 'q') {
		quit(b);
	}

	showmsg(com); // TODO: replace with command call
}

//// INPUT HANDLERS / DRAWS ////

#define EDIT_MOTION(edit, motion) 			\
	s = curpos(b);		\
d = motion; 		\
e = curpos(b);		\
if(d<0) swap(&s, &e);	\
edit(b, s, e);		\
b->linepos = s.p;

int do_motion(buf *b, char c) {// motion is handled here.
	// it returns direction of motion
	switch(c) {
		case 'k':
			return m_prevln(b);
		case 'j':
			return m_nextln(b);
		case 'l':
			return m_nextch(b);
		case 'h':
			return m_prevch(b);
		case 'w':
			return m_nextwrd(b);
		case 'b':
			return m_prevwrd(b);
		case '$':
			return m_eol(b);
		case '^':
			return m_bol(b);
		case 'G': // FIXME
			m_bol(b);
			return m_jump(b, (pos) {b->last, 0});
		case 'g':
			c = getch();
			if(c == 'g')
				return m_jump(b, (pos) {b->first, 0});
		default:
			return 0;
	}
}

void command_mode(buf *b) {
	char c; // character from getch
	pos s;  // start of motion
	pos e;  // end of motion
	int d;  // direction
	while(1) {
		drawbuf(b);
		switch(c = getch()) {
			case 'd':
				c = getch();
				if(c == 'd') // TODO: move somewhere saner
					// TODO: add undo
					delln(b, b->cur);
				else {
					// heh. macros will break here if
					// braces aren't in place... oops
					EDIT_MOTION(e_del, do_motion(b, c));
				}
				clrtoeol();
				break;
			case 'i':
				e_insert(b);
				break;
			case 'x':
				EDIT_MOTION(e_del, m_nextch(b));
				clrtoeol();
				break;
			case 'J':
				e_join(b, (pos) {NULL, 0}, (pos) {NULL, 0});
				clrtoeol();
				break;
			case 'u':
				e_undo(b, (pos) {NULL, 0}, (pos) {NULL, 0});
				break;
			case ' ':
				filestatus(b);
				break;
			case 'o':
				e_new_line(b);
				m_bol(b);
				drawbuf(b); // force redraw
				e_insert(b);
				break;
			case 'O':
				m_prevln(b);
				m_bol(b);
				e_new_line(b);
				drawbuf(b);
				e_insert(b);
				break;
			case ':':
				promptcmd(b);
				break;
			default:
				if(do_motion(b, c)) // assume it's a motion
					break;
		}
	}
}

char *insert_mode(buf *b) { // TODO: refactor
	line *l = b->cur;
	string *r = newstr("", 128); // auto grow string
	char c;
	while((c = getch()) != KEY_ESC) {
		int l = getcurln(b);
		switch(c) {
			case KEY_BS:
				remch(r);
				break;
			default:
				appendch(r, c);
				break;
		}
		move(l, 0);
		clrtoeol();
		addnstr(b->cur->s, b->linepos);
		addstr(r->ss);
		addstr(b->cur->s + b->linepos);
		move(l, b->linepos + r->l);
		refresh();
	}
	return r->ss;
}

void quit(buf *b) {
	freebuf(b);
	endwin();
	delwin(win);
	refresh();
	exit(0);
}

int main(void) {
	if((win = initscr()) == NULL) ERROR("error initializing ncurses");
	noecho();
	scrollok(win, 1);

	buf b = {NULL, NULL, NULL, 0, 0};
	line *n = loadfilebuf(&b, "./pep.c");

	b.cur = b.first;
	b.linepos = 0;
	b.getvlnpos = 0;
	b.scroll = b.first;
	b.undos = NULL;
	b.redos = NULL;

	command_mode(&b);

	quit(&b); // shouldn't ever get reached
	return 0;
}
