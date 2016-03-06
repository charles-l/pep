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
#define MAXLINE 1024  							// maximum possible line length
#define UNDOSIZE 256							// maximum number of undos
#define TABSTOP 5     							// width of tab

#define ERROR(x) fprintf(stderr, "pep: %s", x);
#define KEY_ESC 27    							// escape keycode

typedef struct line { 							// double linked list
	char *s;
	struct line *n;   						// next
	struct line *p;   						// prev
} line;

typedef struct {
	line *l; 		  					// line
	int p;   		  					// linepos
} pos;

typedef struct undo { 							// saves an entire line in the undo list
	pos p;			  					// pointer to position where undo should be inserted
	line l;								// stores changes
	struct undo *n;	  						// next undo (linked list)
} undo;

typedef struct {
	line *first;	  						// first line in file
	line *cur;		  					// current line
	line *last;       						// last line in file
	line *scroll;     						// top of current scroll position
	int linepos;      						// position on line (actual byte position)
	int calcvlnpos;     						// visual line position (to take into account tabs, (future) utf8(?), etc)
	undo *undos;  							// linked list of undos
	undo *redos;  							// linked list of redos
} buf;

void pushundo(buf *b, pos *start, pos *end);	// creates an undo and shoves it into the current buffer undo list
void mvmsg(); 								// move cursor to prompt
char get_c(buf *b);							// get the current character
char get_nc(buf *b);							// get the next character
char get_pc(buf *b);							// get the previous character
int is_eolch(char c);							// check if a character is an eol char
int eos(char *c);							// find end of string
int calcln(buf *b);							// calculate distance between b->scroll and b->cur
int calcvlnpos(buf *b);							// calculate visual position of cursor
int delln(buf *b, line *l);						// used internall with delete edit function
int swap(pos *s, pos *e);						// swap two pos
pos curpos(buf *b); 							// create a pos for current position
line *rngcpy(pos *start, pos *end);					// copy around a range of lines

// motions (must return 1 for forward, -1 for backwards)
int m_nextwrd(buf *b);   						// move to next word
int m_prevwrd(buf *b);
int m_nextch(buf *b);							// next character
int m_prevch(buf *b);
int m_nextln(buf *b);							// next line
int m_prevln(buf *b);
int m_eol(buf *b);							// end of line
int m_bol(buf *b);							// beginning of line
int m_jump(buf *b, pos start);						// jump to position in file

// edits (tracks difference between two motions and operates on inbetween)
int e_del(buf *b, pos start, pos end);					// delete
int e_insert(buf *b);							// insert on line
int e_join(buf *b, pos start, pos end);					// join current and next line
int e_undo(buf *b, pos start, pos end);					// undo

line *load_file(buf *b, const char *fname);
void showmsg(char *s);							// show a message in the status bar
void promptcmd();							// run a command from the prompt
void input(buf *b);							// main input handler
void display(buf *b);							// main draw function

// globals
WINDOW *win;

int is_eolch(char c) {
	return c == '\0' || c == '\n';
}

char get_c(buf *b) {
	return b->cur->s[b->linepos];
}

char get_nc(buf *b) {
	if(get_c(b) == '\0') return '\0';
	return b->cur->s[b->linepos];
}

char get_pc(buf *b) {
	if(b->linepos == 0) return get_c(b);
	return b->cur->s[b->linepos - 1];
}

int calcln(buf *b) {
	int i = 0;
	for(line *l = b->scroll; l != b->cur; l = l->n, i++);
	return i;
}

int calcvlnpos(buf *b) {
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

line *rngcpy(pos *start, pos *end) {
	line *r;
	if(start->l == end->l) {
		r = malloc(end->p - start->p);
		memcpy(r, start->l + start->p, end->p - start->p);
	}
}

void pushundo(buf *b, pos *start, pos *end) {
	undo *u = malloc(sizeof(undo));
	u->p = curpos(b);
	if(start->l == end->l)

	u->n = b->undos;
	b->undos = u;
}

int m_nextch(buf *b) {
	if(!is_eolch(b->cur->s[b->linepos + 1])) {
		b->linepos++;
		return 1;
	}
	return 0;
}

int m_nextwrd(buf *b) {
	if(m_nextch(b))
		if(!isalpha(get_c(b)) && !isspace(get_c(b)))
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
		if(!isalpha(get_c(b)) && !isspace(get_c(b)))
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
	for(b->linepos = 0; !is_eolch(b->cur->s[b->linepos + 1]); b->linepos++);
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
			scrl(-1); // this is a junk call to make sure scrolling doesn't goof display
		}
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
		if(calcln(b) >= LINES - 2) {
			b->scroll = b->scroll->n;
			scrl(1); // so is this one
		}
		if(oldpos > ((int) strlen(b->cur->s) - 2))
			m_eol(b);
		return 1;
	}
	return 0;
}

int eos(char *c) {
	int i = 0;
	while(c[0] != '\0') {c++; i++;}
	return i;
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

char *insert_str(buf *b) { // TODO: refactor
	char *r = malloc(256); 		  // FIXME: could overflow
	r[0] = '\0';
	int i = 1;
	char c;
	while((c = getch()) != KEY_ESC) {
		switch(c) {
			default:
				r[i - 1] = c;
				r[i++] = '\0';
				int l = calcln(b);
				move(l, 0);
				addnstr(b->cur->s, b->linepos);
				addstr(r);
				addstr(b->cur->s + b->linepos);
				move(l, b->linepos + i - 1);
				refresh();
				break;
		}
	}
	return r;
}

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

#define STARTC (start.l->s + start.p)
#define ENDC   (end.l->s + end.p)

int e_del(buf *b, pos start, pos end) {
	pushundo(b, &start, &end);
	if(start.l == end.l)
		if(is_eolch(ENDC[1])) {
			showmsg("eol");
			memmove(STARTC, ENDC, strlen(ENDC) + 1); // TODO: fixme
		} else
			memmove(STARTC, ENDC, strlen(ENDC) + 1);
	else {
		// for visual selection:
		//int i = eos(start.l->s);
		//e_del(b, start, (pos) {start.l, i});
		for(line *l = start.l; l != end.l->n; l = l->n) delln(b, l);
	}
	return 0;
}

int e_insert(buf *b) { // TODO: refactor
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

int e_undo(buf *b, pos start, pos end) {
	if(b->undos != NULL)
		*(b->undos->p.l) = b->undos->l; // TODO: maybe memcpy?
}

pos curpos(buf *b) {
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

#define EDIT_MOTION(edit, motion) s = curpos(b);        \
								  d = motion;	         \
								  e = curpos(b);        \
							      if(d<0) swap(&s, &e);  \
								  edit(b, s, e);         \
								  b->linepos = s.p;

int do_motion (buf *b, char c) { // motion is handled here. it returns direction of motion
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
		case 'G':
			return m_jump(b, (pos) {b->last, 0});
		case 'g':
			c = getch();
			if(c == 'g')
				return m_jump(b, (pos) {b->first, 0});
		default:
			return 0;
	}
}

void input(buf *b) { // other input gets handled here
	char c; // character input
	pos s;  // start
	pos e;  // end
	int d;  // direction
	switch(c = getch()) {
		case 'd':
			c = getch();
			if(c == 'd')
				delln(b, b->cur);
			else { // heh. macros will break here if braces aren't in place... oops
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
		case ':':
			promptcmd();
			break;
		default:
			if(do_motion(b, c))
				break;
	}
}

// TODO: chunk file? if memory is ever an issue, that'll be the easiest thing to do.
line *load_file(buf *b, const char *fname) {
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

#define COMMAND_LEN 256 // max command length

void promptcmd() { // TODO: refactor
	char com[COMMAND_LEN];

	showmsg(":"); // pop prompt

	echo();
	scrollok(win, 0);
	getnstr(com, COMMAND_LEN);
	scrollok(win, 1);
	noecho();

	showmsg(com); // TODO: replace with command call
}

void display(buf *b) {
	line *l = b->scroll;
	b->calcvlnpos = calcvlnpos(b);
	for(int i = 0; l != NULL; l = l->n, i++) {
		if(i > LINES - 2) break; // TODO: don't use LINES to allow for rescaling
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
	move(calcln(b), b->calcvlnpos);
	refresh();
}

int main(void) {
	if((win = initscr()) == NULL) {fprintf(stderr, "error initializing ncurses");}
	noecho();
	scrollok(win, 1);

	buf b = {NULL, NULL, NULL, 0, 0};
	line *n = load_file(&b, "./pep.c");

	b.cur = b.first;
	b.linepos = 0;
	b.calcvlnpos = 0;
	b.scroll = b.first;
	b.undos = NULL;
	b.redos = NULL;

	while(1) {
		display(&b);
		input(&b);
	}

	endwin();
	delwin(win);
	refresh();
	return 0;
}
