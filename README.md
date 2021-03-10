# eff
A Library Provides Extensible Effects in Typed Racket.

The typed version has several limitations due to the design of Typed Racket,so we rewrite it in untyped racket and get a simpler and more efficient design.

We did't use any Any and unsafe operations in the code.

The typed version stem from the design of extensible effects in ocaml (the paper [Eff Directly in OCaml](https://arxiv.org/pdf/1812.11664.pdf)), but it doesn't work
well with typed racket.

## Untyped Version
See branch `untyped`.
We recommend the untyped version for production use.

## Usage
See tests.

## What is `Extensible Effects`?
See [eff lang](https://www.eff-lang.org/).
