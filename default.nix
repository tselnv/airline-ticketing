let
  nixpkgsRev = "19d4f7dc485f74109bd66ef74231285ff797a823"; # stable
  nixpkgsSha = "sha256:1middmy754sc5ni43hqpyppsb7cgnk30g94nzjxmq5jyqkgdl0s8";
  compiler = pkgs.haskellPackeges;
  ghc902 = pkgs.haskell.compiler.ghc902;
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = nixpkgsSha;
  })
  { };
in



pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
      haskellPackages.record-dot-preprocessor
      ormolu
      haskell-language-server
      cabal-install
      ghcid
      pcre
      postgresql
      glibc
  ];
}

