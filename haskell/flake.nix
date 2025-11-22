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

      perSystem = { system, pkgs, ... }:
        let
          hpkgs = pkgs.haskell.packages.ghc912.override (_: {
            overrides = (_: super: {
              ghc = super.ghc.overrideAttrs (old: {
                nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.llvmPackages_19.llvm ];
              });

              network = pkgs.haskell.lib.dontCheck super.network;
            });
          });

          related-post-gen = pkgs.haskell.lib.overrideCabal (hpkgs.callCabal2nix "related-post-gen" ./. { }) (_: {
            doCheck = true;
            doHaddock = false;
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;
          });
        in
        {
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
            ];
          };
        };
    };
}
