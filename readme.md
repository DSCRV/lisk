
![lisk.png](https://u.peppe.rs/0j.png)

`lisk` is a (soon to be) interpreter for R6RS Scheme,
written by the students of RV. It is written in Haskell and
compiled against GHC v8.6.5. `lisk` is an educational
adventure, it does not intend to be highly performant or
fully compliant with R6RS.

### Building and Running `lisk`

On systems using the `nix` package manager:

```shell
cabal2nix . > default.nix
nix-build release.nix
./result/bin/lisk
```

Alternatively, you may build and run using `cabal`:

```shell
# requires ghc & cabal to be installed
cabal build exe:lisk
cabal run lisk
```

### Usage

On running `lisk`, you will be greeted by the `lisk` REPL,
where you may enter `lisk` expressions:

```scheme
;;; Entering lisk repl ...
† (+ 1 1)
2
† (* 42 (- 2 -3))
210
† (and (not (= 2 2)) #f)
#f
```

### Testing

`lisk` includes a property-based testing suite, written with
the QuickCheck module, you may run tests for the project
via:

```shell
cabal run tests
```
