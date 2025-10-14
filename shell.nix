{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShellNoCC {
  packages = with pkgs; [
    opam
  ];

  OPAM = pkgs.opam + "bin/opam";
}

