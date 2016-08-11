CSC=csc
BIN=pep
SOURCE=pep.scm
PREFIX=/usr/local

$(BIN):
	@echo CSC $(BIN)
	@csc $(SOURCE)

clean:
	@rm $(BIN)

install: $(BIN)
	@install $(BIN) $(PREFIX)/bin

uninstall:
	@rm $(PREFIX)/bin/$(BIN)
