#include <stdio.h>
#include <ncurses.h>
#include <unistd.h>

struct buf {
	char *content;
	char *loc;
	int line;
};

void input(struct buf *b) {
	switch(getch())
	{
		case 'k':
			b->line--;
		case 'j':
			b->line++;
	}
}

void display(struct buf *b) {

}

int main(void) {
	WINDOW *win;
	if((win = initscr()) == NULL) {fprintf(stderr, "error initializing ncurses");}
	struct buf b = {NULL, NULL, 0};
	while(1) {
		mvaddstr(0, 0, "content");
		refresh();
		input(&b);
		move(b.line, 0);
	}

	delwin(win);
	endwin();
	refresh();
	return 0;
}
