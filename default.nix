{ pkgs ? import ./dep/nixpkgs {} }:
let 
  inherit (import ./dep/gitignoresrc { inherit (pkgs) lib; }) gitignoreSource;
in 
  pkgs.haskellPackages.developPackage {
    name = "g";
    root = gitignoreSource ./.;
    overrides = self: super: with pkgs.haskell.lib; {
      reflex-fsnotify = 
        doJailbreak 
          (self.callCabal2nix "reflex-fsnotify" 
            (import ./dep/reflex-fsnotify/thunk.nix) {});
    };
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          cabal-fmt
          ghcid
          haskell-language-server
        ]);
  }
