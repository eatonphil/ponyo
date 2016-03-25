.PHONY: all build-static build-server build-documentation deploy configure clean

all:
	$(MAKE) build-server
	$(MAKE) build-static
	$(MAKE) build-documentation

build-static:
	(cd $$(pwd)/static && gulp)

build-server:
	@mkdir -p dist/bin
	ponyo make server/server.sml -o dist/bin/server

build-documentation:
	ponyo doc ../ -o dist/templates/documentation \
            -p dist/templates/documentation/signature.html \
	    -r github.com,eatonphil,ponyo

deploy:
	rsync --exclude dist/bin/server -r dist ponyo:~/

configure:
	@mkdir -p dist/bin
	@(cd $$(pwd)/static && npm install)

clean:
	rm -rf dist static/node_modules
