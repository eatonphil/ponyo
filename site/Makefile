.PHONY: all build-static build-server build-documentation deploy configure clean

OUTPUT_DIR=dist/bin

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
	@(cd $$(pwd)/static && npm install)

clean:
	rm -rf dist static/node_modules
