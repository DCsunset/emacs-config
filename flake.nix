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

  outputs = inputs: let
    inherit (inputs.nixpkgs) lib;
    systems = [ "x86_64-linux" "aarch64-linux" ];

    forAllSystems = f:
      lib.genAttrs
        systems
        (system: f (
          import inputs.nixpkgs {
            inherit system;
            overlays = with inputs; [
              nur-dcsunset.overlays.pkgs
              emacs-overlay.overlays.package
            ];
          }
        ));
  in {
    packages = forAllSystems (pkgs: rec {
      gui = pkgs.callPackage ./emacs.nix {
        dc-lib = inputs.nur-dcsunset.lib;
        extraEpkgs = pkgs.nur-dcsunset.emacsPackages;
        emacs = pkgs.emacs30;
      };
      nox = pkgs.callPackage ./emacs.nix {
        dc-lib = inputs.nur-dcsunset.lib;
        extraEpkgs = pkgs.nur-dcsunset.emacsPackages;
        emacs = pkgs.emacs30-nox;
      };
      default = gui;
    });
  };
}
