{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShellNoCC {
  packages = with pkgs; [
    gnumake
    opam
  ];

  OPAM = pkgs.opam + "bin/opam";
}
