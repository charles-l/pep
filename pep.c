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
#define TABSTOP 5     			// width of tab

///////////

#define ERROR(x) fprintf(stderr, "pep: %s", x);
#define KEY_ESC 27    							// escape keycode

typedef struct line { 							// double linked list
	char *s;
	struct line *n;   							// next
	struct line *p;   							// prev
} line;

typedef struct {
	line *l; 		  							// line
	int p;   		  							// linepos
} pos;

typedef struct undo { 							// saves an entire line in the undo list
	line *pos;		  							// pointer to position where undo should be inserted
	line *changed;	  							// stores changes
	struct undo *n;	  							// next undo (linked list)
} undo;

typedef struct {
	line *first;	  							// first line in file
	line *cur;		  							// current line
	line *last;       							// last line in file
	line *scroll;     							// top of current scroll position
	int linepos;      							// position on line (actual byte position)
	int vlinepos;     							// visual line position (to take into account tabs, (future) utf8(?), etc)
	undo *undos;	  							// linked list of undos
	undo *redos;	  							// linked list of redos
} buf;

char get_c(buf *b);								// get the current character
char get_nc(buf *b);							// get the next character
char get_pc(buf *b);							// get the previous character
int eol_char(char c);							// check if a character is an eol char
int eos(char *c);								// find end of string
int disp_line(buf *b);							// calculate distance between b->scroll and b->cur
int vlinepos(buf *b);							// calculate visual position of cursor
int delete_line(buf *b, line *l);				// used internall with delete edit function
int swap(pos *s, pos *e);						// swap two pos
pos get_pos(buf *b); 							// create a pos for current position
void mv_message(); 								// move cursor to prompt

// motions (must return 1 for forward, -1 for backwards)
int m_next_word(buf *b);   						// move to next word
int m_prev_word(buf *b);
int m_next_char(buf *b);						// next character
int m_prev_char(buf *b);
int m_next_line(buf *b);						// next line
int m_prev_line(buf *b);
int m_eol(buf *b);								// end of line
int m_bol(buf *b);								// beginning of line
int m_jump(buf *b, pos start);					// jump to position in file

// edits (tracks difference between two motions and operates on inbetween)
int e_delete(buf *b, pos start, pos end);		// delete
int e_insert(buf *b);							// insert on line
int e_join(buf *b, pos start, pos end);			// join current and next line

void input(buf *b);								// input handler
line *load_file(buf *b, const char *fname);
void display_message(char *s);					// show a message in the status bar
void display(buf *b);							// main draw function
void run_command();								// run a command from the prompt

// globals
WINDOW *win;

int eol_char(char c) {
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

int disp_line(buf *b) { // calculate (number) the display line of the b->cur
	int i = 0;
	for(line *l = b->scroll; l != b->cur; l = l->n, i++);
	return i;
}

int vlinepos(buf *b) { // calculate the current visual x position of the cursor
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

int m_next_char(buf *b) {
	if(!eol_char(b->cur->s[b->linepos + 1])) {
		b->linepos++;
		return 1;
	}
	return 0;
}

int m_next_word(buf *b) {
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

int m_prev_word(buf *b) {
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

int m_prev_char(buf *b) {
	if(b->linepos > 0) {
		b->linepos--;
		return -1;
	}
	return 0;
}

int m_eol(buf *b) {
	for(b->linepos = 0; !eol_char(b->cur->s[b->linepos + 1]); b->linepos++);
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

int m_prev_line(buf *b) {
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

int m_next_line(buf *b) {
	if(b->cur->n) {
		int oldpos = b->linepos;
		b->cur = b->cur->n;
		if(disp_line(b) >= LINES - 2) {
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

int delete_line(buf *b, line *l) {
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
				int l = disp_line(b);
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
	delete_line(b, b->cur->n);
	free(b->cur->s);
	b->cur->s = s;
	return 0;
}

#define STARTC (start.l->s + start.p)
#define ENDC   (end.l->s + end.p)

int e_delete(buf *b, pos start, pos end) {
	if(start.l == end.l)
		if(eol_char(ENDC[1])) {
			display_message("eol");
			memmove(STARTC, ENDC, strlen(ENDC) + 1); // TODO: fixme
		} else
			memmove(STARTC, ENDC, strlen(ENDC) + 1);
	else {
		// for visual selection:
		//int i = eos(start.l->s);
		//e_delete(b, start, (pos) {start.l, i});
		for(line *l = start.l; l != end.l->n; l = l->n) delete_line(b, l);
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

pos get_pos(buf *b) {
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

int do_motion (buf *b, char c) { // motion is handled here. it returns direction of motion
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
				delete_line(b, b->cur);
			else { // heh. macros will break here if braces aren't in place... oops
				EDIT_MOTION(e_delete, do_motion(b, c));
			}
			clrtoeol();
			break;
		case 'i':
			e_insert(b);
			break;
		case 'x':
			EDIT_MOTION(e_delete, m_next_char(b));
			clrtoeol();
			break;
		case 'J':
			e_join(b, (pos) {NULL, 0}, (pos) {NULL, 0});
			clrtoeol();
			break;
		case ':':
			run_command();
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

void mv_message() { // move to the message box
	move(LINES - 1, 0);
}

void display_message(char *s) { // display a message in the prompt box
	mv_message();
	clrtoeol();
	addstr(s);
	wredrawln(win, LINES - 1, 1);
}

#define COMMAND_LEN 256 // max command length

void run_command() { // TODO: refactor
	char com[COMMAND_LEN];

	display_message(":"); // pop prompt

	echo();
	scrollok(win, 0);
	getnstr(com, COMMAND_LEN);
	scrollok(win, 1);
	noecho();

	display_message(com); // TODO: replace with command call
}

void display(buf *b) {
	line *l = b->scroll;
	b->vlinepos = vlinepos(b);
	for(int i = 0; l != NULL; l = l->n, i++) {
		if(i > LINES - 2) break; // TODO: don't use LINES to allow for rescaling
		move(i, 0);
		for(char *c = l->s; c[0] != '\0'; c++) {
			switch(c[0]) {
				case '\t':
					for(int i = 0; i < TABSTOP; i++)
						addch(' ');
					break;
				case '\n':
					addstr("$");
					break;
				default:
					addch(c[0]);
					break;
			}
		}
	}
	move(disp_line(b), b->vlinepos);
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
	b.vlinepos = 0;
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
