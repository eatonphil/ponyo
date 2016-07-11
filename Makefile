.PHONY: all bootstrap test

all:
	@mkdir -p bin
	# Build ponyo-make
	$(MAKE) bootstrap
	# Build ponyo tool
	bin/ponyo-make tool/ponyo/ponyo.sml -o bin/ponyo
	# Use ponyo-make through ponyo tool
	bin/ponyo make build.sml -C tool/ponyo/doc -o $$(pwd)/bin/ponyo-doc
	bin/ponyo make tool/ponyo/top/top.sml -o bin/ponyo-top

bootstrap:
	@mkdir -p bin
	polyc -o bin/ponyo-make tool/ponyo/make/build.sml

test:
	ponyo-make build.sml -C test -o $$(pwd)/bin/ponyo-test
	@clear
	$$(pwd)/bin/ponyo-test
	rm $$(pwd)/bin/ponyo-test

clean:
	rm -rf bin
