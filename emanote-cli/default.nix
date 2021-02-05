{ pkgs ? import ./dep/nixpkgs {} }:
let 
  inherit (import ./dep/gitignoresrc { inherit (pkgs) lib; }) gitignoreSource;
  hp = (pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      reflex-fsnotify = 
        doJailbreak 
          (self.callCabal2nix "reflex-fsnotify" 
            (import ./dep/reflex-fsnotify/thunk.nix) {});
    };
  }).extend (pkgs.haskell.lib.packageSourceOverrides {
    emanote-core = gitignoreSource ./../emanote-core;
    emanote-cli = gitignoreSource ./.;
    reflex-dom-pandoc = import ./dep/reflex-dom-pandoc/thunk.nix;
    pandoc-link-context = import ./dep/pandoc-link-context/thunk.nix;
  });
  shell = hp.shellFor {
    packages = p: [ p.emanote-cli ];
    buildInputs = 
      [ hp.cabal-install
        hp.cabal-fmt
        hp.ghcid
        hp.haskell-language-server
      ];
  };
in 
  if pkgs.lib.inNixShell then shell else hp.emanote-cli
