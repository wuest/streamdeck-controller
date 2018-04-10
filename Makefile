.PHONY: deps clean all

SRC_DIR     := "src"
ELM_DIR     := "$(SRC_DIR)/elm"
ELM_SOURCES := $(shell find $(ELM_DIR) -name '*.elm')
HS_SOURCES  := $(shell find $(SRC_DIR) -name '*.hs')

.PHONY: clean all deps
.DEFAULT_GOAL := all

all: .cabal-sandbox/bin/streamdeck-controller

.cabal-sandbox/bin/streamdeck-controller: deps static/main.js $(HS_SOURCES)
	cabal build
	cabal install

.cabal-sandbox:
	cabal sandbox init

deps: .cabal-sandbox
	cabal install --only-dependencies

static/main.js: $(ELM_SOURCES)
	elm-make src/elm/Main.elm --output static/main.js --yes

clean:
	rm -rf elm-stuff
	rm -f static/*.js
	rm -rf .cabal-sandbox
