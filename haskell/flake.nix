{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    { nixpkgs
    , flake-parts
    , ...
    }@inputs: flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      perSystem = { system, ... }:
        let
          pkgs = import nixpkgs {
            localSystem = { inherit system; };

            overlays = [
              (final: prev: {
                ghcUseLlvm = (prev.haskell.packages.ghc912.ghc.override {
                  stdenv = final.llvmPackages_19.stdenv;
                }).overrideAttrs (old: {
                  nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ final.llvmPackages_19.llvm ];
                });
              })
            ];
          };

          hpkgs = pkgs.haskell.packages.ghc912.override {
            ghc = pkgs.ghcUseLlvm;
            overrides = _: super: {
              network = pkgs.haskell.lib.dontCheck super.network;
            };
          };

          related-post-gen = pkgs.haskell.lib.overrideCabal (hpkgs.callCabal2nix "related-post-gen" ./. { }) (_: {
            doCheck = true;
            doHaddock = false;
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;
          });
        in
        {
          _module.args.pkgs = pkgs;

          packages.default = related-post-gen;

          devShells.default = pkgs.mkShell {
            buildInputs = [
              hpkgs.cabal-install
              # hpkgs.haskell-language-server
              hpkgs.fourmolu
              hpkgs.ghcid
              hpkgs.ghc
              pkgs.haskellPackages.cabal-fmt
              pkgs.haskellPackages.implicit-hie
              pkgs.llvmPackages_19.llvm
            ];
          };
        };
    };
}
