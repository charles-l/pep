#ifndef CONFIG_H
#define CONFIG_H

#define TABSTOP 4     			// width of tab
#define STATUS_LENGTH 256		// max length of status text
#define COMMAND_LEN 256			// max command length
#define SEARCH_COMMAND "grep -aon '%s'"
#define LINEN_COL COLPAIR(COLOR_GRAY, COLOR_BLACK)

int lnn_width = 5; // width of line number column

#endif // CONFIG_H
