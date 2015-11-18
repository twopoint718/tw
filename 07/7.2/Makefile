SOURCES = BasicSyntax.elm Tests.elm
EXE = ./node_modules/elm-test/bin/elm-test

tests: $(SOURCES) $(EXE)
	./node_modules/elm-test/bin/elm-test Tests.elm

$(EXE): check
	npm install

check: please_install_npm

please_install_npm: ;	@which npm > /dev/null
