{ pkgs }:

with pkgs;

devshell.mkShell {
  name = "tetroid-devshell";
  motd = ''
    Tetroid devshell
  '';
  commands = [
    {
      name = "nixpkgs-fmt";
      help = "use this to format the Nix code";
      category = "fmt";
      package = "nixpkgs-fmt";
    }
  ];

  bash = {
    extra = ''
      export LD_INCLUDE_PATH="$DEVSHELL_DIR/include"
      export LD_LIB_PATH="$DEVSHELL_DIR/lib"
    '';
    interactive = '''';
  };

  packages = [
    tetroid.tetroid.haskell.packages.ghc865.cabal-install
    (tetroid.tetroid.haskell.packages.ghc865.ghcWithPackages (p: with p; [
      aeson
      aeson-pretty
      http-types
      zlib
      unliftio
      wai
      wai-logger
      wai-extra
      servant
      servant-server
      jsaddle
      jsaddle-warp
      transformers
      warp
      websockets
      servant-jsaddle
      miso-jsaddle
      lens
      text
      http-proxy
      http-client
      mtl
      tetroid
    ]))

    binutils
    pkgconfig
    openssl
    openssl.dev
    gcc
    glibc
    gmp.dev
    nixpkgs-fmt
  ];
}
