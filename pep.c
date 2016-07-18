// vim: sw=8 ts=8
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ncurses.h>
#include <unistd.h>
#include <assert.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <ctype.h>
#include <limits.h>
#include <sys/wait.h>
#include "sds.h"

// load config
#include "config.h"

/////

#define QERROR(x) { fprintf(stderr, "pep: %s\n", x); exit(1); }
#define ERROR(x) { fprintf(stderr, "pep: %s\n", x); quit(&b); }
#define KEY_ESC 0x1B			// escape keycode
#define KEY_BS 0x7F    			// backspace keycode
#define KEY_CF 6			// ctrl-f keycode
#define KEY_CG 7			// ctrl-g keycode
#define KEY_CB 2			// ctrl-b keycode

#define CHOPN(s) s[strlen(s)-1]='\0'    // chop newline
#define COLOR_GRAY 8
#define COLPAIR(fg, bg) fg, bg		// hehehe abusing macro preprocessor

// set/unset/check buffer flags
#define SETFLAG(buf, flag) buf->flags |= flag
#define UNSETFLAG(buf, flag) buf->flags &= ~(flag)
#define ISFLAG(buf, flag) (buf->flags & flag)

typedef struct line { 			// double linked list
	char *s;
	struct line *n;   		// next
	struct line *p;   		// prev
} line;

enum undo_t {DELETED, CHANGED};

typedef struct undo { 			// saves an entire line in the undo list
	struct undo *n;	  		// next undo (linked list)
	line *l;			// undo data
	int ln;				// line number
	int offset;			// starting position offset
	enum undo_t type;
} undo;

enum buf_flags {
	MODIFIED = 1 << 0,
	RO 	 = 1 << 1,
};

typedef struct {
	const char *filename;		// buffers filename
	undo *undos;  			// linked list of undos
	line *first;	  		// first line in file
	line *cur;	  		// current line
	line *last;       		// last line in file
	line *scroll;     		// top of current scroll position
	int linepos;      		// byte position on line
	int flags;			// file flag (set by buf_flags)
} buf;

int is_eolch(char c);
line *dupln(line *l);
void freeln(line **l);
int delln(buf *b, line *l);

char curch(buf *b);
char nextch(buf *b);
char prevch(buf *b);
int eos(char *c);

int getlnn(buf *b, line *l);
int getcurlnn(buf *b);
int getvlnpos(char *s, int pos);

int jmpln(buf *b, int i);
int swap(line **s, line **e);

char *chomp(char *s);
char *findsubstr(char *s, char *sub);
char *joinstr(char *a, char *b);
line *lncpy(line *start, line *end);
void pushundo(buf *b, line *start, line *end, int offset, enum undo_t t);

int m_nextch(buf *b);
int m_nextchf(buf *b); // force step to the next character (for line ending)
int m_nextwrd(buf *b);
int m_prevwrd(buf *b);
int m_prevch(buf *b);
int m_eol(buf *b);
int m_bol(buf *b);
int m_jump(buf *b, line *start);
int m_jumpn(buf *b, int ln);
int m_smartbol(buf *b);
int m_prevln(buf *b);
int m_nextln(buf *b);
int m_boscr(buf *b);
int m_eoscr(buf *b); int m_nextscr(buf *b);
int m_prevscr(buf *b);

int e_join(buf *b, line *start, line *end, int _a, int _b);
int e_del(buf *b, line *start, line *end, int s, int e);

int yankln(buf *b, line *l);

int e_yank(buf *b, line *start, line *end, int s, int e);
int e_paste(buf *b);

char *insrtstr(char *s, char *i, int p);

int e_insert(buf *b);
line *newln(char *s);
line *insln(buf *b, line *p, line *s);
line *inslnb(buf *b, line *p, line *s); // insert line before
int e_new_line(buf *b);
int e_undo(buf *b, line *start, line *end, int _a, int _b);

buf *readbuf(FILE *f, const char *fname); // read from a file pointer
buf *loadfilebuf(const char *fname);
void writefilebuf(buf *b, const char *fname);
void freebuf(buf **b);

void drawnstr(WINDOW *w, char *s, int n);
void drawstr(WINDOW *w, char *s);
void drawbuf(buf *b);

int searchnext(buf *b, line *start);
int searchprev(buf *b);

void filestatus(buf *b);
void showmsg(char *s);
char *readprompt(char *pfmt);
void promptcmd(buf *b);
int do_motion(buf *b, char c);
void cmdmode(buf *b);

buf *p_insert(buf *b, FILE *f);
buf *p_none(buf *b, FILE *f);
buf *p_replace(buf *b, FILE *f);
buf *p_hiddenbuf(buf *b, FILE *f);

buf *pipebuf(buf *b, char *cmd, buf * (*fun)(buf *b, FILE *f));
char *insertstr(char *s, char *i, int p);
buf *newbuf(void);

void insmode(buf *b);
void quit(buf **b);
int main(int argc, char **argv);

// globals
WINDOW *win;
WINDOW *linenum;
WINDOW *prompt;
buf *search = NULL; // search buffer
int motion_chain = 0;

int is_eolch(char c) {
	return c == '\0' || c == '\n';
}

line *dupln(line *l) {
	line *r = malloc(sizeof(line));
	r->s = strdup(l->s);
	r->p = NULL;
	r->n = NULL;
	return r;
}

void freeln(line **l) {
	if(!(*l)) return;
	free((*l)->s);
	free(*l);
	*l = NULL;
}

int delln(buf *b, line *l) {
	if(!l || b->first == b->last) return 0;
	if(l == b->first) b->first = b->first->n;
	if(l == b->last)  b->last = b->last->p;
	if(l->p) l->p->n = l->n ? l->n : NULL;
	if(l->n) l->n->p = l->p ? l->p : NULL;

	if(l == b->cur)    b->cur    = b->cur->n ? b->cur->n : b->cur->p;
	if(l == b->scroll) b->scroll = b->scroll->n ? b->scroll->n : b->scroll->p;

	freeln(&l);
	m_bol(b);
	return 1;
}

char curch(buf *b) {
	return b->cur->s[b->linepos];
}

char nextch(buf *b) {
	if(curch(b) == '\0') return '\0';
	return b->cur->s[b->linepos + 1];
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

int getlnn(buf *b, line *l) {
	int i = 1;
	for(line *t = b->first; t && (t != l); t = t->n, i++);
	return i;
}

int getcurlnn(buf *b) {
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

int jmpln(buf *b, int i) {
	b->cur = b->first;
	while(i-- > 0)
		m_nextln(b);
}

int swap(line **s, line **e) {
	line *t;
	t = *e;
	*e = *s;
	*s = t;
	return 0;
}

char *chomp(char *s) { // destructive
	char *r = s + strlen(s);
	while(r != s && isspace((--r)[0])) r[0] = '\0';
	return s;
}

char *findsubstr(char *s, char *sub) {
	size_t subn = strlen(sub);
	size_t sn = strlen(s);
	if(subn > sn) return NULL;
	for(int i = 0; s[i] == sub[i]; i++) {
		putchar(i);
		if(s[i + 1] == '\0') return NULL; // no match in string
		if(sub[i + 1] == '\0') return s; // matched til end of substr
	}
	return findsubstr(s + 1, sub);
}

char *joinstr(char *a, char *b) { // destructive
	size_t i = strlen(chomp(a)) + strlen(chomp(b));
	char *s = malloc(i + 1);
	strcpy(s, a);
	strcat(s, b);
	return s;
}

line *lncpy(line *start, line *end) {
	line *r = dupln(start);
	line *p = r;
	while(start != end) {
		p->n = dupln(start->n);
		p->n->p = r;
		r->n->n = NULL;
		start = start->n;
	}
	return r;
}

void pushundo(buf *b, line *start, line *end, int offset, enum undo_t t) {
	undo *u = malloc(sizeof(undo));
	u->ln = getlnn(b, start) - 1; // getlnn is 1-based
	u->offset = offset;
	u->n = b->undos;
	u->type = t;
	u->l = lncpy(start, end);
	b->undos = u;
}

//// MOTIONS ////

int m_nextch(buf *b) {
	if(nextch(b) != '\0') {
		b->linepos++;
		return 1;
	}
	return 0;
}

int m_nextchf(buf *b) {
	if(curch(b) != '\0') {
		b->linepos++;
		return 1;
	}
	return 0;
}

int m_nextwrd(buf *b) {
	if(m_nextchf(b))
		if(!isspace(curch(b))
				&& ((isspace(prevch(b)) && !isspace(curch(b)))
					|| (isalpha(prevch(b)) && !isalpha(curch(b)))
					|| (!isalpha(prevch(b)) && isalpha(curch(b)))))
			return 1;
		else
			return m_nextwrd(b);
	else {
		if(!motion_chain) {
			m_nextln(b);
			m_bol(b);
			return 1;
		}
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
		if(m_prevln(b))
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
	for(b->linepos = 0; !is_eolch(curch(b)); b->linepos++);
	return 1;
}

int m_bol(buf *b) {
	b->linepos = 0;
	return -1;
}

int m_jump(buf *b, line *start) { // ignores end
	b->cur = start;
	b->scroll = start;
	return 1; // TODO: this isn't always forward
}

int m_jumpn(buf *b, int ln) {
	line *l = b->first;
	for(int i = 1; (l = l->n) && (i < ln - 1); i++);
	b->cur = l;
	b->scroll = l;
	return 1;
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
			wscrl(win, -1); // this is a junk call to make sure
		} // scrolling doesn't goof display
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
		if(getcurlnn(b) >= LINES - 1) {
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
	for(int i = getcurlnn(b); i < LINES - 2 && m_nextln(b); i++);
}

int m_nextscr(buf *b) {
	m_eoscr(b);
	m_nextln(b);
	b->scroll = b->cur;
	return 1;
}

int m_prevscr(buf *b) {
	m_boscr(b);
	for(int i = 0; i < LINES - 1 && m_prevln(b); i++);
	m_eoscr(b);
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
		pushundo(b, start, end, b->linepos, DELETED);
		for(line *l = start; l != end->n; l = l->n) delln(b, l);
	}
	b->linepos = s;
	return 0;
}

int yankln(buf *b, line *l) {
	buf *y = newbuf();
	sds s = sdsnew("\n");
	sdscat(s, l->s);
	insln(y, y->first, newln(s));
	sdsfree(s);
	delln(y, y->first); // remove blank line
	pipebuf(y, "xsel -ib", p_none);
	freebuf(&y);
}

int e_yank(buf *b, line *start, line *end, int s, int e) {
	if(start == end) {
		buf *y = newbuf();
		insln(y, y->first, newln(strndup(start->s + s, e - s)));
		delln(y, y->first); // remove blank line
		pipebuf(y, "xsel -ib", p_none);
		freebuf(&y);
	} else {
		// TODO: implement
	}
	b->linepos = s;
	return 0;
}

int e_paste(buf *b) {
	buf *p = newbuf();
	pipebuf(p, "xsel -ob", p_insert);
	if(p->first->n->s[0] == '\n') {
		insln(b, b->cur, newln(""));
		m_nextln(b);
	}
	char *n = insertstr(b->cur->s, p->first->n->s, b->linepos + 1);
	b->cur->s = n;
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

int e_insert(buf *b) {
	drawbuf(b);
	pushundo(b, b->cur, b->cur, b->linepos, CHANGED);
	insmode(b);
	return 0;
}

line *newln(char *s) {
	line *l = malloc(sizeof(line));
	l->s = strdup(s);
	l->n = NULL;
	l->p = NULL;
	return l;
}

line *insln(buf *b, line *c, line *l) {
	line *e = l;
	while(e->n) e = e->n; // e to the end of the l list

	if(c->n) {
		e->n = c->n;
		c->n->p = e;
	} else {
		e->n = NULL;
		b->last = e;
	}
	l->p = c;
	c->n = l;
	return l;
}

line *inslnb(buf *b, line *c, line *l) {
	if(c->p) {
		l->p = c->p;
		c->p->n = l;
	} else {
		l->p = NULL;
		b->first = l;
	}
	l->n = c;
	c->p = l;
	if(b->scroll == c)
		b->scroll = l;
	return l;
}

int e_new_line(buf *b) {
	b->cur = insln(b, b->cur, newln(""));
	if(getcurlnn(b) == LINES - 1) {
		b->scroll = b->scroll->n;
		scrl(1);
	}
	return 0;
}

int e_undo(buf *b, line *start, line *end, int _a, int _b) {
	if(b->undos) {
		jmpln(b, b->undos->ln);

		if(b->undos->type == CHANGED) {
			delln(b, b->cur);
			b->cur = inslnb(b, b->cur, b->undos->l);
		} else if(b->undos->type == DELETED) { // TODO: fix for `dj` at EOF
			m_prevln(b);
			b->cur = insln(b, b->cur, b->undos->l);
			drawbuf(b);
		}

		b->linepos = b->undos->offset;

		undo *u = b->undos;
		b->undos = b->undos->n; // unsafeness for cleverness sake (it's not multithreaded - so meh)
		free(u);
		return 1;
	}
	return 0;
}

//// BUFFER FUNCTIONS ////

buf *readbuf(FILE *f, const char *fname) { // read from a file pointer
	buf *b = newbuf();
	freeln(&b->first);
	char s[LINE_MAX];
	if(f)
		for(int i = 0; fgets(s, LINE_MAX, f); i++) {
			line *l = malloc(sizeof(line));
			l->s = chomp(strndup(s, strlen(s) - 1)); // chomp after removing known newline
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
	b->cur = b->first;
	b->scroll = b->first;
	b->filename = fname;
}

// TODO: chunk file? if memory is ever an issue,
// that'll be the easiest thing to do.
buf *loadfilebuf(const char *fname) {
	FILE *f = fopen(fname, "r");
	if(!f) return NULL;
	buf *b = readbuf(f, fname);
	if (f) fclose(f);
	return b;
}

void writefilebuf(buf *b, const char *fname) {
	FILE *f = fopen(fname, "w");
	if(f == NULL) ERROR("unable to open file for writing");
	for(line *i = b->first; i; i = i->n) {
		fprintf(f, "%s\n", i->s);
	}
	fclose(f);
}

void freebuf(buf **b) {
	if(!(*b)) return; // don't try to free NULL
	while((*b)->first) {
		free((*b)->first->s);
		free((*b)->first);
		(*b)->first = (*b)->first->n;
	}
	while((*b)->undos) {
		freeln(&((*b)->undos->l));
		undo *u = (*b)->undos;
		(*b)->undos = (*b)->undos->n;
		free(u);
	}
	*b = NULL;
}

void drawnstr(WINDOW *w, char *s, int n) {
	for(int i = 0; s[0] != '\0' && i < n; s++, i++) {
		switch(s[0]) {
			case '\t':
				for(int i = 0; i < TABSTOP; i++)
					waddch(w, ' ');
				break;
			default:
				waddch(w, s[0]);
				break;
		}
	}
}

void drawstr(WINDOW *w, char *s) {
	drawnstr(w, s, INT_MAX);
}

void drawbuf(buf *b) {
	char linenumfmt[12];
	sprintf(linenumfmt, "%%%ii", lnn_width - 1);
	werase(win);
	werase(linenum);
	line *l = b->scroll;
	int i = 0;
	int lnn = getlnn(b, b->scroll);
	for(; l; l = l->n, i++) {
		if(i > LINES - 2) break;
		wmove(win, i, 0);
		drawstr(win, l->s);
		wattron(linenum, COLOR_PAIR(1));
		mvwprintw(linenum, i, 0, linenumfmt, lnn + i);
		wattroff(linenum, COLOR_PAIR(1));
	}
	if(i < LINES - 2) { // fill empty lines with '~'
		wclrtobot(win);
		for(; i < LINES - 2; i++)
			mvwaddch(win, i, 0, '~');
	}
	wrefresh(linenum);
	wmove(win, getcurlnn(b), getvlnpos(b->cur->s, b->linepos));
	wrefresh(win);
}

// TODO: move, since they're technically motions (rename and fix, etc)
// TODO: also through search code in do_motion
int searchnext(buf *b, line *start) {
	if(!search) return 0;
	if(!start) start = b->first;
	if(search->cur->n)
		search->cur = search->cur->n;
	else if(search->cur == search->last)
		search->cur = search->first;

	if(search->cur && (strlen(search->cur->s) > 0)) {
		int i = atoi(search->cur->s);
		if (i < getlnn(b, start)) // don't jump *before* cursor
			return searchnext(b, start);
		m_jumpn(b, atoi(search->cur->s));
		char *m = search->cur->s + 1;
		while(m[-1] != ':') m++; // hehehe look ma i'm clever
		b->linepos = findsubstr(b->cur->s, m) - b->cur->s;
		return 1;
	}
	return 0;
}

int searchprev(buf *b) {
	if(!search) return 0;
	if(search->cur->p)
		search->cur = search->cur->p;
	else if(search->cur == search->first)
		search->cur = search->last;

	if(search->cur) {
		m_jumpn(b, atoi(search->cur->s));
		return 1;
	}
	return 0;
}

//// PROMPT ////

void filestatus(buf *b) {
	char s[STATUS_LENGTH];
	s[0] = '\0';
	strncat(s, b->filename, STATUS_LENGTH);
	if(ISFLAG(b, MODIFIED)) strncat(s, "*", STATUS_LENGTH);
	sprintf(s, "%s (%i%%)", s, (int) (100 * ((float) getlnn(b, b->cur) / getlnn(b, b->last))));
	showmsg(s);
}

void showmsg(char *s) {
	wclear(prompt);
	waddstr(prompt, s);
	wrefresh(prompt);
}

char *readprompt(char *pfmt) {
	char *r = malloc(COMMAND_LEN);
	showmsg(pfmt);
	echo();
	wgetnstr(prompt, r, COMMAND_LEN);
	noecho();
	return r;
}

void promptcmd(buf *b) { // TODO: refactor
	char *com = readprompt(":");

	if(com[0] == 'q') {
		quit(&b);
	} else if(com[0] == 'w') {
		if(strlen(com) <= 2) {
			if(!b->filename) {
				showmsg("no filename specified!");
				wgetch(prompt);
				return;
			}
			writefilebuf(b, b->filename);
		} else {
			char *n = com;
			while(isspace((++n)[0]));
			b->filename = n;
			writefilebuf(b, n);
		}
		UNSETFLAG(b, MODIFIED);
	}

	free(com);
}

//// INPUT HANDLERS / DRAWS ////

#define EDIT_MOTION(edit, motion)\
	motion_chain = 1; \
	ss = b->linepos; s = b->cur;	\
d = motion;			\
ee = b->linepos; e = b->cur;	\
if(d<0) {			\
	swap(&s, &e);		\
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
			if(wgetch(win) == 'g')
				return m_jump(b, b->first);
		case KEY_CF:
			return m_nextscr(b);
		case KEY_CB:
			return m_prevscr(b);
		default:
			return 0;
	}
}

void cmdmode(buf *b) {
	char c; 		// character from getch
	line *s, *e;		// start, end of motion
	int ss, ee, d;		// start, end offest, and direction
	char *i;		// command input string
	char com[COMMAND_LEN];	// command string
	int r;
	while(1) {
		drawbuf(b);
		motion_chain = 0;
		switch(c = wgetch(win)) {
			case 'd':
				c = wgetch(win);
				if(c == 'd') { // TODO: move somewhere saner
					// TODO: replace this with a call to e_del
					pushundo(b, b->cur, b->cur, b->linepos, DELETED);
					delln(b, b->cur);
				} else {
					// heh. macros will break here if
					// braces aren't in place... oops
					EDIT_MOTION(e_del, do_motion(b, c));
				}
				break;
			case 'p':
				e_paste(b);
				break;
			case 'i':
				e_insert(b);
				break;
			case 'I':
				m_bol(b);
				e_insert(b);
				break;
			case 'a':
				m_nextch(b);
				e_insert(b);
				break;
			case 'A':
				m_eol(b);
				e_insert(b);
				break;
			case 'x':
				EDIT_MOTION(e_del, m_nextchf(b));
				break; case 'J':
				e_join(b, NULL, NULL, 0, 0);
				break;
			case 'o':
				e_new_line(b);
				m_bol(b);
				e_insert(b);
				break;
			case 'O':
				b->cur = inslnb(b, b->cur, newln(""));
				e_insert(b);
				break;
			case '|':
				i = readprompt("|");
				pipebuf(b, i, p_insert);
				free(i);
				break;
			case '!':
				i = readprompt("!");
				pipebuf(b, i, p_replace);
				free(i);
				break;
			case 'y':
				c = wgetch(win);
				if(c == 'y') {
					yankln(b, b->cur);
				} else {
					EDIT_MOTION(e_yank, do_motion(b, c));
				}
				goto cleanup;
				break;
			case 'u':
				if(!e_undo(b, NULL, NULL, 0, 0) || !b->undos)
					UNSETFLAG(b, MODIFIED);
				goto cleanup;
				break;
			case 'H': // TODO: move to motions
				m_boscr(b);
				goto cleanup;
				break;
			case 'L':
				m_eoscr(b);
				goto cleanup;
				break;
			case '/': // TODO: move to motion
				i = readprompt("/");
				sprintf(com, SEARCH_COMMAND, i);
				free(i);
				freebuf(&search); // delete previous search
				search = pipebuf(b, com, p_hiddenbuf);
				searchnext(b, b->cur);
				delln(search, search->first); // remove blank newline (it's been jumped now)
				goto cleanup;
				break;
			case 'n':
				searchnext(b, NULL);
				goto cleanup;
				break;
			case 'N':
				searchprev(b);
				goto cleanup;
				break;
			case ':':
				promptcmd(b);
				goto cleanup;
				break;
			case KEY_CG:
				filestatus(b);
				goto cleanup;
				break;
			case 'Z':
				switch(wgetch(win)) {
					case 'Q':
						quit(&b);
					case 'Z':
						writefilebuf(b, b->filename);
						quit(&b);
				}
			default:
				// TODO: properly handle numbers
				if(do_motion(b, c)) // assume it's a motion
					goto cleanup;
		}
		SETFLAG(b, MODIFIED);
		// TODO: clean this up and remove goto
cleanup:
		if(curch(b) == '\0') m_prevch(b); // correct the location if we need to
	}
}

buf *p_insert(buf *b, FILE *f) {
	char lnbuf[LINE_MAX];
	while(fgets(lnbuf, LINE_MAX, f) > 0) {
		CHOPN(lnbuf);
		insln(b, b->cur, newln(lnbuf));
	}
	return NULL;
}

buf *p_none(buf *b, FILE *f) { // essentially a NOP
	return NULL;
}

buf *p_replace(buf *b, FILE *f) {
	char lnbuf[LINE_MAX];
	buf *n = newbuf();
	while(fgets(lnbuf, LINE_MAX, f) > 0) {
		CHOPN(lnbuf);
		delln(b, b->first);
		insln(n, n->last, newln(lnbuf));
	}
	delln(n, n->first); // remove empty line
	freebuf(&b);
	*b = *n;
	return NULL;
}

buf *p_hiddenbuf(buf *b, FILE *f) { // capture output of command in new buffer
	char lnbuf[LINE_MAX];
	buf *n = newbuf();
	while(fgets(lnbuf, LINE_MAX, f) > 0) {
		CHOPN(lnbuf);
		insln(n, n->last, newln(lnbuf));
	}
	return n;
}

// remember to call clrtobot after this function so screen doesn't garbage up
// WHEN IN DOUBT BRUTE FORCE! Copy the entire stinkin' file to a new buf for now.
buf *pipebuf(buf *b, char *cmd, buf * (*fun)(buf *b, FILE *f)) {
	line *l = b->first;

	int p[2];  // first pipe
	int pp[2]; // second pipe
	FILE *f;

	pipe(p);
	pipe(pp);

	switch(fork()) {
		case -1:
			// TODO: use better error handling
			perror("fork");
			break;
		case 0:
			switch(fork()) {
				case -1:
					perror("fork");
					break;
				case 0:
					close(pp[0]);
					close(pp[1]);
					close(p[0]);
					do {
						write(p[1], l->s, strlen(l->s));
						write(p[1], "\n", 1);
					} while (l = l->n);
					write(p[1], "\0", 1); // TODO: find if this is actually needed
					exit(0); // end child process
				default:
					close(p[1]);
					close(pp[0]);
					dup2(p[0], STDIN_FILENO);
					dup2(pp[1], STDOUT_FILENO);
					execl("/bin/sh", "sh", "-c", cmd, NULL);
					exit(0); // end child process (if it gets here)
			}
		default:
			close(pp[1]);
			close(p[0]);
			close(p[1]);

			f = fdopen(pp[0], "r"); // it's easier to fgets
			return fun(b, f);
	}
}

char *insertstr(char *s, char *i, int p) { // NOTE: frees s
	size_t l = strlen(s) + strlen(i);
	char *n = malloc(l);
	strncpy(n, s, p);
	n[p] = '\0';
	strcat(n, i);
	strcat(n, s + p);
	free(s);
	return n;
}

buf *newbuf(void) {
	buf *b = malloc(sizeof(buf));
	b->first = newln("");
	b->last = b->first;
	b->cur = b->first;
	b->scroll = b->cur;
	b->linepos = 0;
	b->undos = NULL;
	b->filename = NULL;
	b->flags = 0; // zero out flags
	return b;
}

// I hate this macro. It's a hcak in place to prevent
// duplicated stuff in the insert function
#define END_INSERT \
	char *n = insertstr(b->cur->s, r, b->linepos);	\
b->cur->s = n;					\
b->linepos = b->linepos + strlen(r) - 1;	\
sdsfree(r);

void insmode(buf *b) {
	sds r = sdsnew("");
	char c;
	while((c = wgetch(win)) != KEY_ESC) {
		int l = getcurlnn(b);
		if(c == KEY_BS) {
			sdslen(r) > 1 ?  sdsrange(r, 0, -2) : sdsclear(r);
		} else if (c == '\n') {
			END_INSERT;
			e_new_line(b);
			m_bol(b);
			drawbuf(b); // force redraw (since we're jumping right into the next insert mode)
			return insmode(b);
		} else {
			char b[2] = {c, '\0'}; // string with new character
			sdscat(r, b);
		}
		wmove(win, l, 0);
		wclrtoeol(win);
		drawnstr(win, b->cur->s, b->linepos);
		drawstr(win, r);
		drawstr(win, b->cur->s + b->linepos);
		wmove(win, l, getvlnpos(b->cur->s, b->linepos) + getvlnpos(r, sdslen(r)));
		wrefresh(win);
	}
	END_INSERT;
}

void quit(buf **b) {
	endwin();
	freebuf(b);
	freebuf(&search);
	delwin(win);
	delwin(linenum);
	delwin(prompt);
	exit(0);
}

int main(int argc, char **argv) {
	if(initscr() == NULL) QERROR("error initializing ncurses");
	start_color();

	win = newwin(LINES - 1, COLS - lnn_width, 0, lnn_width);
	linenum = newwin(LINES - 1, lnn_width, 0, 0);
	prompt = newwin(1, COLS, LINES - 1, 0);

	init_pair(1, LINEN_COL);

	noecho();
	scrollok(win, 1);

	buf *b = argc < 2 ? newbuf() : loadfilebuf(argv[1]);
	if(!b) {
		endwin();
		QERROR("failed to load/create file buf");
	}

	cmdmode(b);
	return 0;
}
