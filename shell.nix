let

  all-hies = fetchTarball {
    url = "https://github.com/infinisil/all-hies/tarball/534ac517b386821b787d1edbd855b9966d0c0775";
    sha256 = "0bw1llpwxbh1dnrnbxkj2l0j58s523hjivszf827c3az5i4py1i2";
  };

  pkgs = import <nixpkgs> {
    # Pass no config for purity
    config = {};
    overlays = [
      (import all-hies {}).overlay
    ];
  };

  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    base
    lens
    parsec
    mtl
    readline
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  externalPackages = [
    ghc
    pkgs.gdb
    pkgs.cabal2nix
    haskellPackages.cabal-install
    haskellPackages.hie
    haskellPackages.hoogle
  ];
in
pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = externalPackages;
}
