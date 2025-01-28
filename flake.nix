{
  description = "My personal Emacs configuration in Nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    nur-dcsunset = {
      url = "github:DCsunset/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ nixpkgs, flake-parts, nur-dcsunset, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" ];

      imports = [
        flake-parts.flakeModules.easyOverlay
      ];

      perSystem = { self', system, pkgs, lib, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ nur-dcsunset.overlays.pkgs ];
        };
        packages = {
          default = pkgs.callPackage ./emacs.nix {
            dc-lib = nur-dcsunset.lib;
            extraEpkgs = pkgs.nur-dcsunset.emacsPackages;
          };
        };
      };
    };
}
