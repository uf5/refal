{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports =
        [ inputs.haskell-flake.flakeModule inputs.treefmt-nix.flakeModule ];

      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          devShell = {
            hoogle = false;
          };
        };

        treefmt.config = {
          projectRootFile = "flake.nix";

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;

          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = { };
        };

        packages.default = self'.packages.refal;
      };
    };
}
