# Ponyo

Ponyo is a comprehensive high-level library and toolkit for Standard ML. It is inspired
by [POCO](http://pocoproject.org/), the [Go standard library](https://golang.org/pkg/),
and the [Python standard library](https://docs.python.org/3/library/). While
the Standard ML basis library is (perhaps) surprisingly solid, there are
still a number of gaps to fill there and in the Standard ML ecosystem in general.

Ponyo is geared toward use on the server as a safe scripting language and for
server-side web development. As such, Ponyo provides additional tools to ease
development - including build and documentation generation tools. Ponyo
(the library and tooling) is intended to be useful both as a whole and piece by piece.
For instance, the documentation generator can be used to generate documentation
for any Standard ML project. The build tool can be used to manage any Poly/ML
compilation. And the Standard ML parser can be used as a compiler frontend.
Down the line, Ponyo could include code generation tools for LLVM, BEAM, or the JVM.

Ponyo is intended to work on [Poly/ML](https://github.com/polyml/polyml) and Unix
systems. See below for [why Standard ML](#why-standard-ml).

## Features

Ponyo is a work in progress. For a roadmap of modules, see ROADMAP.md

* ponyo-doc: generates HTML from signatures (currently)
  * as the Standard ML parser matures, it will not need to parse signatures
* ponyo-build: high-level tool for building (Ponyo) projects
* containers
  * binary search tree
* HTTP client and server
* Standard ML parser
  * AST export
* and much more!

## Modules overview

* Container
  * Tree
    * [BinarySearchTree](https://github.com/eatonphil/ponyo/blob/master/ponyo/Container/Tree/BinarySearchTree.sml)
* [Format](https://github.com/eatonphil/ponyo/blob/master/ponyo/Format/FormatExport.sml)
* Net
  * [Http](https://github.com/eatonphil/ponyo/tree/master/ponyo/Net/Http)
* Os
  * [File](https://github.com/eatonphil/ponyo/blob/master/ponyo/Os/File.sml)
  * [Cli](https://github.com/eatonphil/ponyo/blob/master/ponyo/Os/Cli/CliExport.sml)
* Sml
  * [Lexer](https://github.com/eatonphil/ponyo/blob/master/ponyo/Sml/Lexer.sml)
  * [Parser](https://github.com/eatonphil/ponyo/blob/master/ponyo/Sml/Parser.sml)
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