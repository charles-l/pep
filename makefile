BIN=pep
SOURCES=pep.scm
PREFIX=/usr/local

#TODO: write proper compile code
$(BIN): $(SOURCES)
	@csc $(SOURCES) -o $(BIN)

clean:
	@rm $(BIN)

install: $(BIN)
	@install $(BIN) $(PREFIX)/bin

uninstall:
	@rm $(PREFIX)/bin/$(BIN)
