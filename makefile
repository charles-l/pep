CC=gcc
PREFIX=/usr/local/

CFLAGS=-g
LDFLAGS=-lncurses

BIN=pep
SOURCES=pep.c
OBJS=$(SOURCES:.c=.o)

$(BIN): $(OBJS)
	@echo LD $<
	@cc -o $(BIN) $(CFLAGS) $(LDFLAGS) $<

.c.o:
	@echo CC $<
	@$(CC) -o $@ -c $< $(CFLAGS)

install: $(BIN)
	@install $(BIN) $(PREFIX)/bin

uninstall:
	@rm $(PREFIX)/bin/$(BIN)
