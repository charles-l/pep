#include <curses.h>

int main() {
	initscr();
	int i = getch();
	endwin();
	printf("%i\n", i);
}
