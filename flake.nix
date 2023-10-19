{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:

  flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in {
      devShell = with pkgs; mkShellNoCC {
        name = "xmonad";
        buildInputs = [
          haskell-language-server
          (haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
              xmobar
              xmonad
              xmonad-contrib
          ]))
        ];
      };
    });
}
