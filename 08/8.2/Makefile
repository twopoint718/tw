sources=$(wildcard src/*.elm)

all: js/elm.js

js/elm.js: $(sources)
	elm make --warn src/Main.elm $(sources) --output js/elm.js

clean:
	rm js/elm.js
