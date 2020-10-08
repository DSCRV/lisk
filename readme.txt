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

* use Control.Monad.Except from mtl to handle errors
* implement property based testing with quickcheck, look
  under tests/Properties.hs
