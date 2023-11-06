{ servant, miso, servant-jsaddle }:
final: prev:
let
  noCheck = p: final.haskell.lib.dontCheck p;
  noHaddock = p: final.haskell.lib.dontHaddock p;
  fast = p: noHaddock (noCheck p);
  misoPkgs = import miso { system = final.system; allowBroken = true; };
in
{
  tetroid = rec {
    inherit servant servant-jsaddle misoPkgs;
    haskellPkg = (misoPkgs.pkgs.haskell.packages.ghc865.override {
      all-cabal-hashes = misoPkgs.pkgs.fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/90e9a5c0282099dd8aa5a369b2b474d0dc354ab8.tar.gz";
        sha256 = "sha256-2bEC/2b+Fa+yCg72upOHKQtEzCbf6lYjpTN0nT23nZw=";
      };
    }).extend (self: super: {
      aeson = noCheck (self.callHackage "aeson" "1.4.4.0" { });
      clay = self.callHackage "clay" "0.13.3" { };
      websockets = self.callHackage "websockets" "0.12.6.0" { };
      http-client = self.callHackage "http-client" "0.6.4.1" { };
      http-proxy = fast super.http-proxy;
      servant-client-core = self.callHackage "servant-client-core" "0.16" { };
      servant = self.callHackage "servant" "0.16" { };
      servant-server = self.callHackage "servant-server" "0.16" { };
      servant-lucid = self.callHackage "servant-lucid" "0.9" { };
      servant-jsaddle = noCheck (self.callCabal2nix "servant-jsaddle" servant-jsaddle { });
      jsaddle-warp = fast super.jsaddle-warp;
      time-compat = fast (self.callHackage "time-compat" "1.9.2.2" { });
      type-errors = self.callHackage "type-errors" "0.2.0.0" { };
      type-errors-pretty = self.callHackage "type-errors-pretty" "0.0.1.1" { };
      first-class-families = self.callHackage "first-class-families" "0.5.0.0" { };
      th-abstraction = self.callHackage "th-abstraction" "0.3.1.0" { };
      th-lift = self.callHackage "th-lift" "0.8.0.1" { };
      tetroid = self.callCabal2nix "tetroid" ./. { miso = misoPkgs.miso-jsaddle; };
    });
    tetroidHaskell = misoPkgs.pkgs // {
      haskell = misoPkgs.pkgs.haskell // {
        packages = misoPkgs.pkgs.haskell.packages // {
          ghc865 = haskellPkg;
        };
      };
    };
    nix = prev.callPackage ./nix { };
  };
}
