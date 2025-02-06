{
  description = "My personal Emacs configuration in Nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nur-dcsunset = {
      url = "github:DCsunset/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ nixpkgs, emacs-overlay, flake-parts, nur-dcsunset, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" ];

      imports = [
        flake-parts.flakeModules.easyOverlay
      ];

      perSystem = { self', system, pkgs, lib, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            nur-dcsunset.overlays.pkgs
            emacs-overlay.overlays.package
          ];
        };
        packages = {
          default = self'.packages.gui;
          gui = pkgs.callPackage ./emacs.nix {
            dc-lib = nur-dcsunset.lib;
            extraEpkgs = pkgs.nur-dcsunset.emacsPackages;
            emacs = pkgs.emacs30;
          };
          nox = pkgs.callPackage ./emacs.nix {
            dc-lib = nur-dcsunset.lib;
            extraEpkgs = pkgs.nur-dcsunset.emacsPackages;
            emacs = pkgs.emacs30-nox;
          };
        };
      };
    };
}
