SOURCES = src/BasicSyntax.elm src/Functions.elm src/Records.elm src/TypesAndTypeAliases.elm
TESTS = tests/Main.elm tests/Tests.elm
EXE = ./node_modules/elm-test/bin/elm-test

tests: $(SOURCES) $(TESTS) $(EXE)
	./node_modules/elm-test/bin/elm-test

$(EXE): check
	npm install

check: please_install_npm

please_install_npm: ;	@which npm > /dev/null
