OUT = dist
ELM_MAIN = $(OUT)/elm.js
ELM_FILES = $(shell find src -iname "*.elm")

.PHONY: all
all: $(ELM_MAIN) $(OUT)/index.html $(OUT)/index.js

$(ELM_MAIN): $(ELM_FILES) node_modules
	yes | npx elm make src/Main.elm --output $@

$(OUT)/%: src/%
	@cp $< $@

node_modules: package.json package-lock.json
	npm install

.PHONY: test
test: node_modules
	@npx elm-test

.PHONY: watch
watch:
	@find src | entr make

.PHONY: format
format: node_modules
	@npx elm-format --yes src/ tests/

.PHONY: clean
clean:
	@rm -fr $(OUT) elm-stuff tests/elm-stuff node_modules
