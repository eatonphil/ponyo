# Ponyo

Ponyo is a comprehensive high-level library for Standard ML. It is inspired
by [POCO](http://pocoproject.org/), the [Go standard library](https://golang.org/pkg/),
and the [Python standard library](https://docs.python.org/3/library/). It is the first
such library in Standard ML and addresses some of the gaps found in
a standard that has seen little revision in almost 20 years.

In particular, the needs of PL researchers and the average programmer interested in ML
are different. Ponyo's take on a comprehensive library will be geared toward
use on the server as a safe scripting language and for server-side web development.
See below for [why Standard ML](#why-standard-ml).

## Example

```sml
(*
 * This simple program makes an HTTP GET request against http://api.ipify.org/
 * and prints the response.
 *)

structure client = Http.Client
structure request = Http.Request
structure method = Http.Method

fun main () =
    let
        val req = request.new (method.Get, "api.ipify.org", "")
        val rsp = client.act (req)
    in
        print (#body response)
    end
```

## Overview

Modules (WIP):
* archive
  * tar
  * zip
* compress
  * gzip
  * deflate
* container
  * tree
    * [BinarySearchTree](https://github.com/eatonphil/ponyo/blob/master/src/container/tree/binarysearch.sml)
    * [RedBlackTree](https://github.com/eatonphil/ponyo/blob/master/src/container/tree/redblack.sml)
  * map
  * set
* database
* encoding
  * base64
  * json
  * html
  * utf8
  * xml
* log
* math
* mime
* misc
* net
  * [http](https://github.com/eatonphil/ponyo/blob/master/src/net/http/http.sml)
  * smtp
* os
* sml
* [string](https://github.com/eatonphil/ponyo/blob/master/src/string/string.sml)
  * regex
  * template
* test
* time

## Why Standard ML

Standard ML is a high-level, strongly- and statically-typed, garbage-collected,
functional programming language that is expressive and easy to learn. In particular,
it is one of the simplest languages in the ML family (among OCaml and Haskell).
It is also one of the few in this family with a definition and certified compilation.

For the working programmer, Standard ML can be thought of as sitting between Java or
Go and Python or Bash. It is appealing coming from Python or Bash with its threading
support, immutability (by default), and compile-time type-checking. It is appealing
coming from Java or Go with its succinct syntax, higher-order functions, pattern
matching and algebraic datatypes.

## License

2-clause "Simplified BSD" license.