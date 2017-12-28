haskell_lib_source = $(wildcard "src/*.hs")
haskell_exe_source = $(wildcard "app/*.hs")
elm_source = $(wildcard "elm/*.elm")
binary = ./.stack-work/install/x86_64-osx/lts-9.4/8.0.2/bin/exe
frontend = assets/html/elm.html

all : setup build

setup : client-setup server-setup

build : client-build server-build

client-setup:
	elm package install -y

client-build: $(elm_source)
	elm make elm/Main.elm --output $(frontend)

server-setup:
	stack setup
	stack test

server-build:
	stack build

run: setup build generate-elm
	stack exec exe

generate-elm:
	stack exec exe -- gen
