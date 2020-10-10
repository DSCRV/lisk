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


usage
-----

$ lisk
;;; Entering lisk repl ...
(lisk)> (+ 1 2 3)
Right 6
(lisk)> (not (= 2 3))
Right #t
(lisk)> '(a b c)
Right (a b c)



todo
----

apart from lines/blocks marked with TODO in the source
files:

* implement correct double/int interaction (src/Operators.hs)
* implement boolean operations: and, or
* rectify Ord implementation for LispNumber (src/Operators.hs)
* implement property based testing with quickcheck (tests/Properties.hs)
