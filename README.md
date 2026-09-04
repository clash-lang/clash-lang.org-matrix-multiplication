# Matrix multiplication with Clash

Source code accompanying the blog post [Matrix multiplication with
Clash](https://clash-lang.org/blog/2018-07-09-matrix-multiplication/). The
project was created with the `clash-lang/simple` starter project, see
[clash-lang.org/install](https://clash-lang.org/install/) for how to set
up Clash on your machine.

## Layout

| Module                            | Section of the blog post                        |
|-----------------------------------|-------------------------------------------------|
| `MatrixMultiplication.Lists`      | Haskell implementation (lists)                  |
| `MatrixMultiplication.Naive`      | Clash implementation (fully parallel, `Vec`)    |
| `MatrixMultiplication.Matrix`     | Matrix helpers used by the sequential designs   |
| `Clash.Class.Counter` (clash-prelude) | Wrap-around counters on tuples of `Index`   |
| `MatrixMultiplication.Sequential` | Splitting hardware: `mmmult2d` with `mealy`     |
| `MatrixMultiplication.Pipeline`   | Pipelining `dot`: `foldrp`, `foldrpp`, `dotf`   |
| `MatrixMultiplication.Pipelined`  | Putting it together again: reader/dot/writer    |
| `MatrixMultiplication.Top`        | A monomorphic `topEntity` for HDL generation    |

## Building and running

Using [Stack](https://docs.haskellstack.org/en/stable/):

```
stack build
stack test
stack run clashi
stack run clash -- MatrixMultiplication.Top --vhdl
```

Using Cabal:

[Install GHC 9.12](https://www.haskell.org/ghcup/) and set it as your default.

```
cabal update
cabal build
cabal run test-library
cabal run doctests
cabal run clashi
cabal run clash -- MatrixMultiplication.Top --vhdl
```

Generated HDL ends up in `vhdl/`, `verilog/` or `systemverilog/`.
