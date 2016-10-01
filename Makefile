.PHONY: all ssl.so bin/ponyo-make bin/ponyo bin/ponyo-doc bin/ponyo-top clean test
default: all

ssl.so: lib/ssl.c
	gcc -shared -fPIC -o $@ -lcrypto -lssl $<

bin/ponyo-make: tool/ponyo/make/build.sml
	@mkdir -p bin
	polyc -o $@ $<

bin/ponyo: tool/ponyo/ponyo.sml bin/ponyo-make
	@mkdir -p bin
	bin/ponyo-make $< -o $@

bin/ponyo-doc: tool/ponyo/doc/build.sml bin/ponyo
	@mkdir -p bin
	bin/ponyo make $< -o $@

bin/ponyo-top: tool/ponyo/top/top.sml bin/ponyo
	@mkdir -p bin
	bin/ponyo make $< -o $@

all:
	#$(MAKE) ssl.so
	# bootstrap
	$(MAKE) bin/ponyo-make
	# Build ponyo tool
	$(MAKE) bin/ponyo
	# Use ponyo-make through ponyo tool
	$(MAKE) bin/ponyo-top
	$(MAKE) bin/ponyo-doc

test: test/build.sml bin/ponyo
	@mkdir -p bin
	ponyo make $< -o bin/$@
	./bin/$@
	@rm ./bin/$@

clean:
	rm -rf bin ssl.so
