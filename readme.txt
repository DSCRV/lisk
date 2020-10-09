lisk
----

an educational lisp interpreter written in haskell


build and run
-------------

nix:
  
  cabal2nix . > default.nix
  nix-build release.nix
  ./result/bin/lisk


cabal:

  cabal run


todo
----

* implement correct double/int interaction (src/Operators.hs)
* implement boolean operations: and, not, or
* write Ord instance for LispNumber (src/Operators.hs)
* implement property based testing with quickcheck (tests/Properties.hs)
