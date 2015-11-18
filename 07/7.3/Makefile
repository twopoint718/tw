sources=$(wildcard src/*.elm)

all: js/elm.js

js/elm.js: $(sources) Main.elm
	elm make Main.elm $(sources) --output js/elm.js

clean:
	rm js/elm.js
