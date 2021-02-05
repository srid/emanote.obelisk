{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
}:
with obelisk;
project ./. ({ pkgs, hackGet, ... }: {
  android.applicationId = "ca.srid.emanote";
  android.displayName = "Emanote";
  ios.bundleIdentifier = "ca.srid.emanote";
  ios.bundleName = "Emanote";

  packages = {
    emanote-core = hackGet ./emanote-core;
    emanote = hackGet ./emanote;
    with-utf8 = hackGet ./dep/with-utf8;
    pandoc-link-context = hackGet ./dep/pandoc-link-context;
    reflex-dom-pandoc = hackGet ./dep/reflex-dom-pandoc;
    algebraic-graphs = hackGet ./dep/alga;
    relude = hackGet ./dep/relude;
  };
  overrides = 
    self: super: with pkgs.haskell.lib; {
      algebraic-graphs = dontCheck super.algebraic-graphs;
      relude = dontCheck super.relude;
    };
})
