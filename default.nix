{ mkDerivation, base, parsec, readline, stdenv }:
mkDerivation {
  pname = "lisk";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base parsec readline ];
  license = stdenv.lib.licenses.gpl3;
}
