# Ponyo

Ponyo is a comprehensive high-level library for Standard ML. It is inspired
by [POCO](http://pocoproject.org/), the [Go standard library](https://golang.org/pkg/),
and the [Python standard library](https://docs.python.org/3/library/). It is the first
such library in Standard ML and addresses some of the gaps found in
a standard that has seen little revision in almost 20 years.

In particular, there are discrepancies between the needs of PL researchers and the
average programmer interested in ML. Ponyo is geared toward use on the server as a
safe scripting language and for server-side web development. See below for
[why Standard ML](#why-standard-ml).

Additionally, Ponyo brings a commitment to thorough, high-quality documentation. Work on
Ponyo documentation necessarily includes documentation of Ponyo itself and more complete
documentation of the Standard ML basis library as well.

Finally, there are only immediate plans to support [Poly/ML](https://github.com/polyml/polyml)
as the Standard ML implementation of choice.

### Notes

Do not expect a stable API for a while. This library is very exciting but it
has a (very) long way to go.

Documentation is of the highest priority. If you come to this page, please
do not share this repo arbitrarily until we get to the point where the documentation
is good enough for beginners to jump in.

## Examples

Here are some of the most basic things you can do with Ponyo.

### HTTP Server

```sml
(*
 * This simple program serves HTTP requests at localhost:9339.
 * This example can be built by running:
 *
 * $ polyc tests/net/http/server/build.sml # must be in root of ponyo git repo
 *)

structure Server = Ponyo.Net.Http.Server
structure Format = Ponyo.Format

fun main () =
    Server.listenAndServe ("", 9339, (fn (req) =>
        let
            val path = Request.path req
        in
            Response.new (Format.sprintf "Hello world at %s!" [path])
        end
    ))
```

### HTTP Client

```sml
(*
 * This simple program makes an HTTP GET request against http://api.ipify.org/
 * and prints the response. This example can be built by running:
 *
 * $ polyc tests/net/http/client/build.sml # must be in root of ponyo git repo
 *)

structure Client = Ponyo.Net.Http.Client
structure Request = Ponyo.Net.Http.Request
structure Method = Ponyo.Net.Http.Method

structure Format = Ponyo.Format

fun main () =
    let
        val req = Request.new (Method.Get, "", "")
        val rsp = Client.act ("api.ipify.org", req)
    in
        Format.println [Response.body rsp]
    end
```


## Overview

Modules (WIP, see [ROADMAP](https://github.com/eatonphil/ponyo/blob/master/ROADMAP.md)):
* Container
  * Tree
    * [BinarySearchTree](https://github.com/eatonphil/ponyo/blob/master/ponyo/Container/Tree/BinarySearchTree.sml)
* [Format](https://github.com/eatonphil/ponyo/blob/master/ponyo/Format/FormatExport.sml)
* Net
  * [Http](https://github.com/eatonphil/ponyo/tree/master/ponyo/Net/Http)
* Os
  * [File](https://github.com/eatonphil/ponyo/blob/master/ponyo/Os/File.sml)
* [String](https://github.com/eatonphil/ponyo/blob/master/ponyo/String/StringExport.sml)

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