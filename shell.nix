{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {
  buildInputs = [
    pkgs.pre-commit
    pkgs.fd
    pkgs.zlib
    pkgs.nixfmt

    pkgs.pkgsCross.ghcjs.buildPackages.ghc
    pkgs.cabal-install
    pkgs.fourmolu
    pkgs.haskellPackages.doctest
    pkgs.haskellPackages.cabal-doctest
    pkgs.haskellPackages.cabal-fmt

    pkgs.nodejs
    pkgs.miniserve
  ];
}
