{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells.${system}.default = pkgs.mkShellNoCC {
        name = "nixos";

        buildInputs = with pkgs; [
          nixpkgs-fmt
          sops
        ];

        inherit (self.checks.${system}.pre-commit-check) shellHook;
      };

    };
}
