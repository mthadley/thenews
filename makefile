OUT = dist
ELM_MAIN = $(OUT)/elm.js
JS_MAIN = $(OUT)/index.js
ELM_FILES = $(shell find src -iname "*.elm")

all: $(ELM_MAIN) $(JS_MAIN) $(OUT)/index.html

clean:
	@rm -fr $(OUT)

$(ELM_MAIN): $(ELM_FILES)
	elm-make src/Main.elm --warn --output $(ELM_MAIN)

$(JS_MAIN): src/index.js
	@cp src/index.js $(OUT)

$(OUT)/index.html: src/index.html
	@cp src/index.html $(OUT)

test:
	@elm-test

watch:
	@find src | entr make

pages: all
	@echo "Publishing to Github pages..."
