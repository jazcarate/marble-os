{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;
  ghcide-nix = import (fetchTarball https://github.com/cachix/ghcide-nix/tarball/master) { };

  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.ghcid
    ghcide-nix.ghcide-ghc883
    haskellPackages.brittany
  ];
  shellHook = ''
    export NIX_GHC="${project.env.NIX_GHC}"
    export NIX_GHCPKG="${project.env.NIX_GHCPKG}"
    export NIX_GHC_DOCDIR="${project.env.NIX_GHC_DOCDIR}"
    export NIX_GHC_LIBDIR="${project.env.NIX_GHC_LIBDIR}"
  '';
}