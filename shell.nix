let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  inherit (pkgs) haskellPackages;
  all-hls = pkgs.fetchFromGitHub {
    owner = "masaeedu";
    repo = "all-hls";
    rev = "155e57d7ca9f79ce293360f98895e9bd68d12355";
    sha256 = "04s3mrxjdr7gmd901l1z23qglqmn8i39v7sdf2fv4zbv6hz24ydb";
  };

  hls = import all-hls {
    platform = "Linux";
    version = "0.4.0";
    ghc = "8.6.5";
  }; # All parameters are optional. The default values are shown here.


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
    haskellPackages.hoogle
    haskellPackages.hlint
  ];
in
pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = externalPackages ++ [ hls ];
}
