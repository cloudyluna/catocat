{
  description = "A top down game";

  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url  = "github:numtide/flake-utils";
  };
  
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
        {
          devShells.default = with pkgs; pkgs.mkShell rec {
            nativeBuildInputs = [
              haskell.compiler.ghc96
              haskellPackages.cabal-install
              haskellPackages.haskell-language-server
              
              ghciwatch
              haskellPackages.cabal-gild
            ];
            buildInputs = [
              pkg-config
              upx
              tiled

              libGL
              xorg.libX11
              xorg.libXcursor
              xorg.libXext
              xorg.libXi
              xorg.libXinerama
              xorg.libXrandr
              glfw
              
            ];

            LD_LIBRARY_PATH = "${lib.makeLibraryPath buildInputs}";
          
        };
      }
    );
}

