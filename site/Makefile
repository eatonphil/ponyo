.PHONY: all build-static build-server build-documentation deploy configure clean

OUTPUT_DIR=dist/bin

all:
	$(MAKE) server
	$(MAKE) static
	$(MAKE) documentation

static:
	(cd $$(pwd)/static && gulp)

server: server/server.sml
	@mkdir -p $(OUTPUT_DIR)
	ponyo make $< -o $(OUTPUT_DIR)/$@

documentation:
	ponyo doc ../ -o dist/templates/documentation \
            -p dist/templates/documentation/signature.html \
	    -r github.com,eatonphil,ponyo

deploy:
	rsync --exclude $(OUTPUT_DIR)/server -r dist ponyo.org:~/

configure:
	@mkdir -p $(OUTPUT_DIR)
	@(cd $$(pwd)/static && npm install)

clean:
	rm -rf dist static/node_modules
