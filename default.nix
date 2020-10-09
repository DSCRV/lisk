{ mkDerivation, base, mtl, parsec, QuickCheck, readline, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, test-framework-th
}:
mkDerivation {
  pname = "lisk";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base mtl parsec ];
  executableHaskellDepends = [ base mtl parsec readline ];
  testHaskellDepends = [
    base parsec QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2 test-framework-th
  ];
  description = "a lisp interpreter";
  license = stdenv.lib.licenses.gpl3;
}
