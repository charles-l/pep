#include <stdlib.h>
#include <string.h>
#include <ncurses.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <ctype.h>
#include <limits.h>
#include <sys/wait.h>

// configs
#define MAXLINE 1024  			// maximum possible line length
#define TABSTOP 8     			// width of tab
#define STATUS_LENGTH 256		// max length of status text
#define COMMAND_LEN 256 		// max command length

#define ERROR(x) { fprintf(stderr, "pep: %s\n", x); exit(1); }
#define KEY_ESC 0x1B    		// escape keycode
#define KEY_BS 0x7F    			// backspace keycode
#define KEY_CF 6
#define KEY_CB 2

typedef struct line { 			// double linked list
	char *s;
	struct line *n;   		// next
	struct line *p;   		// prev
} line;

enum undo_t {DELETED, CHANGED};

typedef struct undo { 			// saves an entire line in the undo list
	line *p;			// starting position
	int pp;				// starting position offset
	line *l;			// stores changes
	enum undo_t t;
	struct undo *n;	  		// next undo (linked list)
} undo;

typedef struct string {			// auto growing string (only use when necessary)
	char *ss;			// content
	size_t s;			// size
	size_t l;			// length
} string;				// TODO: perhaps throw into its own file?

typedef struct {
	line *first;	  		// first line in file
	line *cur;	  		// current line
	line *last;       		// last line in file
	line *scroll;     		// top of current scroll position
	int linepos;      		// byte position on line
	undo *undos;  			// linked list of undos
	undo *redos;  			// linked list of redos
	const char *filename;		// buffers filename
} buf;

int is_eolch(char c);
int delln(buf *b, line *l);
char curch(buf *b);			// current character
char nextch(buf *b);			// next character
char prevch(buf *b);			// previous character
int eos(char *c);			// distance to end of string
int getcurln(buf *b);			// get visual y position of cursor
int getvlnpos(char *s, int pos);	// get visual x cursor position
int swap(line **s, line **e);
line *lncpy(line *start, line *end);
line *insln(buf *b, line *p, char *s);  // add a new line after p, with content s (s is strdupped)
void pushundo(buf *b, line *start, line *end, int offset, enum undo_t t);
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
int m_jump(buf *b, line *start);
int m_prevln(buf *b);
int m_nextln(buf *b);

int e_join(buf *b, line *start, line *end, int s, int e);
int e_del(buf *b, line *start, line *end, int s, int e);
int e_insert(buf *b);
int e_new_line(buf *b);
int e_undo(buf *b, line *start, line *end, int s, int e);
void bpipe(buf *b, line *start, line *end, char *command, char **args);	// blocking pipe
void drawnstr(char *s, int n);
void drawstr(char *s);
line *loadfilebuf(buf *b, const char *fname);
void writefilebuf(buf *b, const char *fname);
void freebuf(buf *b);
void drawbuf(buf *b);
void filestatus(buf *b);

void mvmsg();
void showmsg(char *s);
void promptcmd(buf *b);

void command_mode(buf *b);
void insert_mode(buf *b);
void quit(buf *b);
// globals
WINDOW *win;

int is_eolch(char c) {
	return c == '\0' || c == '\n';
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

int getvlnpos(char *s, int pos) {
	int i = 0;
	for(int j = 0; j < pos; j++)
		switch(s[j]) {
			case '\t':
				i += TABSTOP;
				break;
			default:
				i++;
				break;
		}
	return i;
}

int swap(line **s, line **e) {
	line *t;
	t = *e;
	*e = *s;
	*s = t;
	return 0;
}

line *lncpy(line *start, line *end) {
	line *r = malloc(sizeof(line));
	r->n = NULL;
	r->p = NULL;
	if(start == end) {
		r->s = malloc(strlen(start->s));
		strcpy(r->s, start->s);
	}
	return r;
}

void pushundo(buf *b, line *start, line *end, int offset, enum undo_t t) {
	undo *u = malloc(sizeof(undo));
	u->p = b->cur;
	u->pp = offset;
	u->n = b->undos;
	u->t = t;
	u->l = lncpy(start, end);
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

int remch(string *s) { // backspace
	if(s->l > 0) {
		s->ss[--s->l] = '\0';
		return 1;
	}
	return 0;
}

void freestr(string *s) {
	free(s->ss);
	free(s);
	s = NULL;
}

//// MOTIONS ////

int m_nextch(buf *b) {
	if(nextch(b) != '\0') {
		b->linepos++;
		return 1;
	}
	return 0;
}

int m_nextwrd(buf *b) {
	if(m_nextch(b))
		if(!isspace(curch(b))
				&& ((isspace(prevch(b)) && !isspace(curch(b)))
					|| (isalpha(prevch(b)) && !isalpha(curch(b)))
					|| (!isalpha(prevch(b)) && isalpha(curch(b)))))
				return 1;
		else
			return m_nextwrd(b);
	else {
		m_nextln(b);
		m_bol(b);
		return 1;
	}
}

int m_prevwrd(buf *b) {
	if(m_prevch(b))
		if(b->linepos == 0 ||
				!isspace(curch(b))
				&& ((isspace(prevch(b)) && !isspace(curch(b)))
					|| (isalpha(prevch(b)) && !isalpha(curch(b)))
					|| (!isalpha(prevch(b)) && isalpha(curch(b)))))
				return -1;
		else
			return m_prevwrd(b);
	else {
		m_prevln(b);
		m_eol(b);
		return -1;
	}
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

int m_jump(buf *b, line *start) { // ignores end
	b->cur = start;
	b->scroll = start;
	clear();
}

int m_smartbol(buf *b) {
	m_bol(b);
	while(isspace(curch(b))) b->linepos++;
	return -1;
}

int m_prevln(buf *b) {
	if(b->cur->p) {
		int oldpos = b->linepos;
		if(b->cur == b->scroll) {
			b->scroll = b->scroll->p;
			scrl(-1); // this is a junk call to make sure
		}			  // scrolling doesn't goof display
		b->cur = b->cur->p;
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
		if(getcurln(b) >= LINES - 1) {
			b->scroll = b->scroll->n;
			scrl(1); // so is this one
		}
		if(oldpos > ((int) strlen(b->cur->s) - 2))
			m_eol(b);
		return 1;
	}
	return 0;
}

int m_boscr(buf *b) {
	b->cur = b->scroll;
	m_bol(b);
	return -1;
}

int m_eoscr(buf *b) {
	for(int i = getcurln(b); i < LINES - 2 && m_nextln(b); i++);
}

int m_nextscr(buf *b) {
	m_eoscr(b);
	m_nextln(b);
	b->scroll = b->cur;
	clear(); // force clear
	return 1;
}

int m_prevscr(buf *b) {
	m_boscr(b);
	for(int i = 0; i < LINES - 1 && m_prevln(b); i++);
	m_eoscr(b);
	clear(); // force clear
	return -1;
}

//// EDITS ////

int e_join(buf *b, line *start, line *end, int _a, int _b) {
	size_t i = strlen(b->cur->s) + strlen(b->cur->n->s);
	char *s = malloc(i + 1);
	strcpy(s, b->cur->s);

	int e = strlen(s);
	if(s[e] == '\n') s[e] = '\0';

	strcat(s, b->cur->n->s);
	delln(b, b->cur->n);
	free(b->cur->s);
	b->cur->s = s;
	return 0;
}

int e_del(buf *b, line *start, line *end, int s, int e) {
	if(start == end) {
		pushundo(b, start, end, b->linepos, CHANGED);
		memmove(start->s + s, end->s + e, strlen(end->s + e) + 1);
	} else {
		for(line *l = start; l != end->n; l = l->n) delln(b, l);
	}
	b->linepos = s;
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
	pushundo(b, b->cur, b->cur, b->linepos, CHANGED);
	insert_mode(b);
	return 0;
}

line *insln(buf *b, line *p, char *s) { // p is the line to insert after, s is the content of the new line
	line *l = malloc(sizeof(line));
	l->n = p->n;
	l->p = p;
	l->s = strdup(s);
	p->n->p = l;
	p->n = l;
	return l;
}

int e_new_line(buf *b) {
	b->cur = insln(b, b->cur, "");
	clrtobot();
	return 0;
}

int e_undo(buf *b, line *start, line *end, int _a, int _b) {
	if(b->undos != NULL) {
		b->undos->l->n = b->undos->p->n;
		b->undos->l->p = b->undos->p->p;

		line *l;
		if(b->undos->t == CHANGED) {
			*(b->undos->p) = *(b->undos->l); // TODO: maybe memcpy?
			l = b->undos->p;
		} else if(b->undos->t == DELETED) {
			l = insln(b, b->undos->p->p, b->undos->l->s);
		}

		b->cur = l;
		b->linepos = b->undos->pp;

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
		l->s = strndup(s, strlen(s) - 1); // strip off '\n'
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
	b->filename = fname;
}

void writefilebuf(buf *b, const char *fname) {
	FILE *f = fopen(fname, "w");
	if(f == NULL) ERROR("unable to open file for writing");
	for(line *i = b->first; i != NULL; i = i->n) {
		fprintf(f, "%s\n", i->s);
	}
	fclose(f);
}

void freebuf(buf *b) {
	while(b->first != NULL) {
		free(b->first->s);
		free(b->first);
		b->first = b->first->n;
	}
	while(b->undos != NULL) {
		free(b->undos->l->s);
		free(b->undos->l);
		free(b->undos);
		b->undos = b->undos->n;
	}
}

void drawnstr(char *s, int n) {
	for(int i = 0; s[0] != '\0' && i < n; s++, i++) {
		switch(s[0]) {
			case '\t':
				for(int i = 0; i < TABSTOP; i++)
					addch(' ');
				break;
			default:
				addch(s[0]);
				break;
		}
	}
}

void drawstr(char *s) {
	drawnstr(s, INT_MAX);
}

void drawbuf(buf *b) {
	line *l = b->scroll;
	int i = 0;
	for(; l != NULL; l = l->n, i++) {
		if(i > LINES - 2) break;
		move(i, 0);
		drawstr(l->s);
	}
	if(i < LINES - 2) { // fill empty lines with '~'
		clrtobot();
		for(; i < LINES - 2; i++)
			mvaddch(i, 0, '~');
	}
	move(getcurln(b), getvlnpos(b->cur->s, b->linepos));
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
	if(com[0] == 'w') {
		writefilebuf(b, b->filename);
	}

	showmsg(com); // TODO: replace with command call
}

//// INPUT HANDLERS / DRAWS ////

#define EDIT_MOTION(edit, motion) 			\
ss = b->linepos; s = b->cur; \
d = motion;		     		 \
ee = b->linepos; e = b->cur; \
if(d<0) { 		     		 \
	swap(&s, &e); 			 \
	if(ss != ee) {ss ^= ee; ee ^= ss; ss ^= ee;} \
} \
edit(b, s, e, ss, ee);

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
			return m_smartbol(b);
		case 'G': // FIXME
			m_bol(b);
			return m_jump(b, b->last);
		case 'g':
			if(getch() == 'g')
				return m_jump(b, b->first);
		case KEY_CF:
			return m_nextscr(b);
		case KEY_CB:
			return m_prevscr(b);
		default:
			return 0;
	}
}

void command_mode(buf *b) {
	char c; 	// character from getch
	line *s, *e;	// start, end of motion
	int ss, ee, d;	// start, end offest, and direction
	char *command[] = {"tr", "a", "b", NULL};
	while(1) {
		drawbuf(b);
		switch(c = getch()) {
			case 'd':
				c = getch();
				if(c == 'd') { // TODO: move somewhere saner
					// TODO: replace this with a call to e_del
					pushundo(b, b->cur, b->cur, b->linepos, DELETED);
					delln(b, b->cur);
				} else {
					// heh. macros will break here if
					// braces aren't in place... oops
					EDIT_MOTION(e_del, do_motion(b, c));
				}
				clrtoeol();
				break;
			case 'i':
				e_insert(b);
				break;
			case 'I':
				m_bol(b);
				drawbuf(b);
				e_insert(b);
				break;
			case 'a':
				m_nextch(b);
				drawbuf(b);
				e_insert(b);
				break;
			case 'A':
				m_eol(b);
				drawbuf(b);
				e_insert(b);
				break;
			case 'x':
				EDIT_MOTION(e_del, m_nextch(b));
				clrtoeol();
				break;
			case 'J':
				e_join(b, NULL, NULL, 0, 0);
				clrtoeol();
				break;
			case 'u':
				e_undo(b, NULL, NULL, 0, 0);
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
			case 'H':
				m_boscr(b);
				break;
			case 'L':
				m_eoscr(b);
				break;
			case ':':
				promptcmd(b);
				break;
			case '|':
				bpipe(b, NULL, NULL, "/usr/bin/tr", command);
				break;
			default:
				if(do_motion(b, c)) // assume it's a motion
					break;
		}
	}
}

//// PIPES ////
// Maybe use this for reading in a file too?
void bpipe(buf *b, line *start, line *end, char *command, char **args) {
	if(start == NULL) {
		int pfd[2]; // pipe file descriptor
		if(pipe(pfd) == -1) ERROR("unable to pipe");
		char buf[MAXLINE];
		switch(fork()) {
			case -1:
				ERROR("unable to fork");
				break;
			case 0: // child
				dup2(pfd[0], 0);
				close(pfd[1]);
				execvp(command, args);
				while(read(pfd[0], buf, MAXLINE)) {
					buf[strlen(buf) - 1] = '\0'; // remove newline
					insln(b, b->cur, buf);
				}
				break;
			default: // parent
				dup2(pfd[1], 1);
				close(pfd[0]);
				for(line *l = b->first; l != NULL; l = l->n) {
					char *m = malloc(strlen(l->s) + 2);
					sprintf(m, "%s\n", l->s);
					write(pfd[1], m, strlen(l->s) + 2);
					free(m);
				}
				break;
		}
	}
	clear();
	drawbuf(b);
}

char *insertstr(char *s, char *i, int p) { // insert string i into s at position p
	size_t l = strlen(s) + strlen(i);
	char *n = malloc(l);
	strncpy(n, s, p);
	n[p] = '\0';
	strcat(n, i);
	strcat(n, s + p);
	return n;
}

// I hate this macro. It's a hcak in place to prevent
// duplicated stuff in the insert function
#define END_INSERT() \
	n = insertstr(b->cur->s, r->ss, b->linepos); \
	free(b->cur->s);				   \
	b->cur->s = n;					   \
	b->linepos = b->linepos + strlen(r->ss) - 1;	   \
	freestr(r);

void insert_mode(buf *b) { // TODO: refactor (and cleanup)
	string *r = newstr("", 128); // auto grow string
	char c;
	char *n;
	int m = 0;
	while((c = getch()) != KEY_ESC) {
		int l = getcurln(b);
		switch(c) {
			case KEY_BS:
				if(!remch(r)) {
					m_prevln(b);
					m = strlen(b->cur->s);
					e_join(b, NULL, NULL, 0, 0);
					clear();
					drawbuf(b);
					l--;
					b->linepos = m;
				}
				break;
			case '\n':
				END_INSERT();
				e_new_line(b);
				m_bol(b);
				drawbuf(b);
				return insert_mode(b);
			default:
				appendch(r, c);
				break;
		}
		move(l, 0);
		clrtoeol();
		drawnstr(b->cur->s, b->linepos);
		drawstr(r->ss);
		drawstr(b->cur->s + b->linepos);
		move(l, getvlnpos(b->cur->s, b->linepos) + getvlnpos(r->ss, r->l));
		refresh();
	}
	END_INSERT();
}

void quit(buf *b) {
	freebuf(b);
	endwin();
	delwin(win);
	refresh();
	exit(0);
}

int main(int argc, char **argv) {
	if((win = initscr()) == NULL) ERROR("error initializing ncurses");
	if(argc < 2) {
		ERROR("No file to open!");
		exit(1);
	}

	noecho();
	scrollok(win, 1);

	buf b = {NULL, NULL, NULL, 0, 0};
	line *n = loadfilebuf(&b, argv[1]);

	b.cur = b.first;
	b.linepos = 0;
	b.scroll = b.first;
	b.undos = NULL;
	b.redos = NULL;

	command_mode(&b);

	quit(&b); // shouldn't ever get reached
	return 0;
}
