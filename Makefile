.PHONY: all ssl.so bin/ponyo-make bin/ponyo bin/ponyo-doc bin/ponyo-top clean test
default: all

ssl.so: lib/ssl.c
	gcc -shared -fPIC -o $@ -lcrypto -lssl $<

bin/ponyo-make: tool/make/build.sml
	@mkdir -p bin
	polyc -o $@ $<

bin/ponyo: tool/ponyo.sml bin/ponyo-make
	@mkdir -p bin
	bin/ponyo-make $< -o $@

bin/ponyo-doc: tool/doc/build.sml tool/doc/generate.sml tool/doc/doc.sml bin/ponyo bin/ponyo-make
	@mkdir -p bin
	bin/ponyo make $< -o $@

bin/ponyo-top: tool/top/top.sml bin/ponyo bin/ponyo-make
	@mkdir -p bin
	bin/ponyo make $< -o $@

bin/ponyo-test: tool/test/test.sml bin/ponyo bin/ponyo-make
	@mkdir -p bin
	bin/ponyo-make $< -o $@

all:
	#$(MAKE) ssl.so
	# bootstrap
	$(MAKE) bin/ponyo-make
	# Build ponyo tool
	$(MAKE) bin/ponyo
	# Use ponyo-make through ponyo tool
	$(MAKE) bin/ponyo-top
	$(MAKE) bin/ponyo-doc

test: test/*.sml bin/ponyo-test
	@mkdir -p bin
	ponyo test

clean:
	rm -rf bin ssl.so
