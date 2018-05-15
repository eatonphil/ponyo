# Ponyo

[![CircleCI](https://circleci.com/gh/eatonphil/ponyo.svg?style=svg)](https://circleci.com/gh/eatonphil/ponyo)

Ponyo is a comprehensive high-level library and toolkit for Standard ML.
It is inspired by POCO (C++), the Go standard library, and the Python standard
library. While the Standard ML basis library is (perhaps) surprisingly
solid, there are still a number of gaps to fill there and in the Standard
ML ecosystem in general.

See [ponyo.org](http://ponyo.org) for more information, documentation,
tutorials, and news.

## Prereqs

### Poly/ML

Install [Poly/ML](https://github.com/polyml/polyml). You will also need
OpenSSL development headers and GNU Make.

### MLton

Install [MLton](https://github.com/MLton/mlton). MLton is a bit of a
second-class citizen but that should improve over time. In particular,
the green-threaded synchronous HTTP server is lousy.

#### SSL client sockets

SSL client socket support requires special compilation and is not yet
supported by the regular Ponyo build process. To build Ponyo with SSL
client socket support for MLton, run:

```
mlton -link-opt -lssl -link-opt -lcrypto <YOURPROJECT>.mlb lib/ssl.c
```

## Installation

Next, choose a path for the Ponyo root. We will assume it is at ~/vendor/ponyo.
Add the following lines to your ~/.profile:

```bash
PONYO_ROOT=~/vendor/ponyo
```

Now, download and install Ponyo to the path.

```bash
$ cd ~/vendor
$ git clone https://github.com/eatonphil/ponyo
$ cd ponyo
$ make
$ sudo make install
```

## License

2-clause "Simplified BSD" license.
