{
  description = "todomvc-nix";
  # To update all inputs:
  # $ nix flake update --recreate-lock-file
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.devshell.url = "github:numtide/devshell/main";

  inputs.servant = { url = "github:haskell-servant/servant"; flake = false; };
  inputs.servant-jsaddle = { url = "github:haskell-servant/servant-jsaddle/master"; flake = false; };
  inputs.miso = { url = "github:dmjio/miso/master"; flake = false; };

  outputs = { self, nixpkgs, flake-utils, devshell, servant, miso, servant-jsaddle }:
    {
      overlay = import ./overlay.nix { inherit servant miso servant-jsaddle; };
    }
    //
    (
      flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
        let
          pkgs = import nixpkgs {
            inherit system;

            config = {
              allowBroken = true;
              permittedInsecurePackages = [
                "openssl-1.0.2u"
              ];
            };
            overlays = [
              devshell.overlays.default
              self.overlay
            ];
          };
        in
        {
          legacyPackages = pkgs.tetroid;

          packages = flake-utils.lib.flattenTree pkgs.tetroid;

          devShell = import ./devshell.nix { inherit pkgs; };

          checks = { };
        }
      )
    );
}
