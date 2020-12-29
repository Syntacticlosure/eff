# eff
A Library Provides Extensible Effects in Typed Racket.

The typed version has several limitations due to the design of Typed Racket.

We did't use any Any and unsafe operations in the code.
## Progress
We are redesigning this library to let it more efficient and simpler for typed racket(in `dev` branch).

The old version stem from the design of extensible effects in ocaml (the paper [Eff Directly in OCaml](https://arxiv.org/pdf/1812.11664.pdf)), but it doesn't work
well with typed racket.

## Usage
See tests.

## What is `Extensible Effects`?
See [eff lang](https://www.eff-lang.org/).
