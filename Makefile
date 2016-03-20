.PHONY: all bootstrap

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
	$$(pwd)/test/run_all.sh

clean:
	rm -rf bin
