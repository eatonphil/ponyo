OUTPUT_DIR=dist/bin

.PHONY: all static $(OUTPUT_DIR)/server reference deploy configure clean

all:
	$(MAKE) $(OUTPUT_DIR)/server
	$(MAKE) static
	$(MAKE) reference

static:
	(cd $$(pwd)/static && ./node_modules/.bin/gulp)

$(OUTPUT_DIR)/server: server/server.sml
	@mkdir -p $(OUTPUT_DIR)
	ponyo make $< -o $@

reference:
	ponyo doc ../ -o dist/templates/reference \
            -p dist/templates/reference/signature.html \
	    -r github.com/eatonphil/ponyo

deploy:
	rsync --exclude $(OUTPUT_DIR)/server -r dist ponyo.org:~/

configure:
	@mkdir -p $(OUTPUT_DIR)
	@(cd $$(pwd)/static && yarn)

clean:
	rm -rf dist static/node_modules
