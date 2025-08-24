{
  description = "notion-to-memgraph flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskellPackages;
        notion-to-memgraph = haskellPackages.callCabal2nix "notion-to-memgraph" ./. {};
      in {
        packages.default = notion-to-memgraph;
        devShells.default = pkgs.mkShell {
          inputsFrom = [ notion-to-memgraph ];
        };
      }
    );
}
