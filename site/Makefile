.PHONY: all

all:
	$(MAKE) build-static
	$(MAKE) build-server

build-static:
	(cd $$(pwd)/static && gulp)

build-server:
	@mkdir -p dist/bin
	ponyo make server/server.sml -o dist/bin/server

clean:
	rm -rf bin dist
