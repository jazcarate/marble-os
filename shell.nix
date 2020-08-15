{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;
  unstable = import (fetchTarball https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) { };

  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.ghcid
    unstable.haskellPackages.ghcide # ghcide 0.2.0 needed for multicradle #hie.yaml
    haskellPackages.brittany
  ];
  shellHook = ''
    export NIX_GHC="${project.env.NIX_GHC}"
    export NIX_GHCPKG="${project.env.NIX_GHCPKG}"
    export NIX_GHC_DOCDIR="${project.env.NIX_GHC_DOCDIR}"
    export NIX_GHC_LIBDIR="${project.env.NIX_GHC_LIBDIR}"
  '';
}