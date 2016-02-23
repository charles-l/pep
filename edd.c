#include <stdio.h>
#include <ncurses.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>

// TODO: use mmap to read portion of file rather than the entire stinkin' thing

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
			break;
		case 'j':
			b->line++;
			break;
	}
}

void display(struct buf *b) {

}

int main(void) {
	WINDOW *win;
	if((win = initscr()) == NULL) {fprintf(stderr, "error initializing ncurses");}
	noecho();
	struct buf b = {NULL, NULL, 0};

	int fd = open("./edd.c", O_RDONLY);
	size_t len = lseek(fd, 0, SEEK_END);
	char *file = (char *) mmap(0, len, PROT_READ, MAP_PRIVATE, fd, 0);

	while(1) {
		mvaddstr(0, 0, file);
		move(b.line, 0);
		refresh();
		input(&b);
	}

	endwin();
	delwin(win);
	refresh();
	return 0;
}
