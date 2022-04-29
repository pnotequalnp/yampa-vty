{
  inputs = {
    nixpkgs.url = "nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides =
            final.lib.composeExtensions prev.haskell.packageOverrides
            (hsFinal: hsPrev: {
              yampa-vty = hsFinal.callCabal2nix "yampa-vty" ./. { };
            });
        };
      };
    in flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        inherit (pkgs.lib) composeExtensions optional;
        inherit (pkgs.haskell.packages) ghc8107 ghc921;
        tools = with ghc921; [ cabal-install fourmolu hlint pkgs.nixfmt ];
        devShell = hs:
          hs.shellFor {
            packages = hsPkgs: with hsPkgs; [ yampa-vty ];
            nativeBuildInputs = tools ++ [ hs.haskell-language-server ];
          };
      in {
        packages = {
          ghc921 = ghc921.yampa-vty;
          ghc8107 = ghc8107.yampa-vty;
          default = ghc921.yampa-vty;
        };

        devShells = {
          default = devShell ghc921;
          ghc921 = devShell ghc921;
          ghc8107 = devShell ghc8107;
          ci = pkgs.mkShell { nativeBuildInputs = tools; };
        };
      }) // {
        overlays.default = overlay;
      };
}
