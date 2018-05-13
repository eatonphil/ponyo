OUTPUT_DIR=dist/bin

.PHONY: all static $(OUTPUT_DIR)/server documentation deploy configure clean

all:
	$(MAKE) $(OUTPUT_DIR)/server
	$(MAKE) static
	$(MAKE) documentation

static:
	(cd $$(pwd)/static && ./node_modules/.bin/gulp)

$(OUTPUT_DIR)/server: server/server.sml
	@mkdir -p $(OUTPUT_DIR)
	ponyo make $< -o $@

documentation:
	ponyo doc ../ -o dist/templates/documentation \
            -p dist/templates/documentation/signature.html \
	    -r github.com/eatonphil/ponyo

deploy:
	rsync --exclude $(OUTPUT_DIR)/server -r dist ponyo.org:~/

configure:
	@mkdir -p $(OUTPUT_DIR)
	@(cd $$(pwd)/static && yarn)

clean:
	rm -rf dist static/node_modules
