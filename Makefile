.PHONY: all ssl.so bin/ponyo-make bin/ponyo bin/ponyo-doc bin/ponyo-top clean test

default: all

UNAME:=$(shell uname)

GCC_INCLUDES=
ifeq ($(UNAME), Darwin)
	GCC_INCLUDES="-I/usr/local/opt/openssl/include"
endif

SML_BACKEND?="polyml"

ssl.so: lib/ssl.c
	gcc $(GCC_INCLUDES) -shared -fPIC -o $@ -lcrypto -lssl $<

bin/ponyo-make: tool/make/build.sml
	@mkdir -p bin
	polyc -o $@ $<

bin/ponyo: tool/ponyo.sml bin/ponyo-make
	@mkdir -p bin
	bin/ponyo-make $< -b $(SML_BACKEND) -o $@

bin/ponyo-doc: tool/doc/build.mlb tool/doc/generate.sml tool/doc/doc.sml bin/ponyo bin/ponyo-make
	@mkdir -p bin
	bin/ponyo-make tool/doc/doc.sml -f tool/doc/generate.sml -b $(SML_BACKEND) -o $@

bin/ponyo-top: tool/top/top.sml bin/ponyo bin/ponyo-make
	@mkdir -p bin
	bin/ponyo-make $< -b polyml -o $@

bin/ponyo-test: tool/test/test.sml bin/ponyo bin/ponyo-make
	@mkdir -p bin
	bin/ponyo-make $< -b $(SML_BACKEND) -o $@

all:
	$(MAKE) ssl.so
	$(MAKE) bin/ponyo-make
	$(MAKE) bin/ponyo
	$(MAKE) bin/ponyo-top
	$(MAKE) bin/ponyo-doc

install:
	ln -s bin/* /usr/local/bin

uninstall:
	rm /usr/local/bin/ponyo*

test: test/*.sml bin/ponyo-test
	@mkdir -p bin
	ponyo-test -b $(SML_BACKEND)

clean:
	rm -rf bin ssl.so
