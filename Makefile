.PHONY: all bootstrap

all:
	@mkdir -p bin
	$(MAKE) bootstrap
	bin/ponyo-make tool/ponyo/ponyo.sml -o bin/ponyo
	bin/ponyo make build.sml -C tool/ponyo/doc -o $$(pwd)/bin/ponyo-doc

bootstrap:
	@mkdir -p bin
	polyc -o bin/ponyo-make tool/ponyo/make/build.sml

test:
	$$(pwd)/test/run_all.sh

clean:
	rm -rf bin
