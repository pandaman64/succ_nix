# https://github.com/srid/rust-nix-template/blob/b35f2df597ae7da9c87c68b4b72eefcb6b3b8dfa/flake.nix
{
  description = "Success type system for Nix";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
    crate2nix = {
      url = "github:kolloch/crate2nix";
      flake = false;
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, utils, rust-overlay, crate2nix, flake-compat }:
    let
      name = "succ_nix";
      rustChannel = "stable";
    in
    utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              rust-overlay.overlay
              (self: super: {
                rustc = self.rust-bin.${rustChannel}.latest.default;
                cargo = self.rust-bin.${rustChannel}.latest.default;
              })
            ];
          };
          inherit (import "${crate2nix}/tools.nix" { inherit pkgs; }) generatedCargoNix;

          crate = pkgs.callPackage
            (generatedCargoNix {
              inherit name;
              src = ./.;
            })
            { };
          runTests = false;
        in
        rec {
          packages.${name} = crate.rootCrate.build.override {
            inherit runTests;
          };

          # nix build
          defaultPackage = packages.${name};

          # nix run
          apps.${name} = utils.lib.mkApp {
            inherit name;
            drv = packages.${name};
          };
          defaultApp = apps.${name};

          # nix develop
          devShell = pkgs.mkShell {
            inputsFrom = builtins.attrValues self.packages.${system};
            buildInputs = [
              pkgs.nixpkgs-fmt
              pkgs.rust-bin.${rustChannel}.latest.rust-analysis
              pkgs.rust-bin.${rustChannel}.latest.rls
              pkgs.cargo-edit
            ];
            RUST_SRC_PATH = "${pkgs.rust-bin.${rustChannel}.latest.rust-src}/lib/rustlib/src/rust/library";
          };
        }
      );
}
