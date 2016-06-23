CC=gcc
PREFIX=/usr/local/

CFLAGS=-g
LDFLAGS=-lncurses

BIN=pep
SOURCES=pep.c astr.c
OBJS=$(SOURCES:.c=.o)

$(BIN): $(OBJS)
	@echo LD $(OBJS)
	@cc -o $(BIN) $(CFLAGS) $(LDFLAGS) $(OBJS)

.c.o:
	@echo CC $<
	@$(CC) -o $@ -c $< $(CFLAGS)

clean:
	@rm $(BIN) $(OBJS)

install: $(BIN)
	@install $(BIN) $(PREFIX)/bin

uninstall:
	@rm $(PREFIX)/bin/$(BIN)
