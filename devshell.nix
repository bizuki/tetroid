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

  env = [
    # {
    #   name = "GMP_LIB_DIR";
    #   value = "${gmp.out}/lib";
    # }
    # {
    #   name = "GMP_INCLUDE_DIR";
    #   value = "${gmp.out.dev}/include";
    # }
    {
      name = "LD_INCLUDE_PATH";
      value = "$DEVSHELL_DIR/include:${gmp.out.dev}/include";
    }
    {
      name = "LD_LIB_PATH";
      value = "$DEVSHELL_DIR/lib:${gmp.out}/lib";
    }
  ];

  bash = {
    # extra = ''
    #   export LD_INCLUDE_PATH="$DEVSHELL_DIR/include:$GMP_INCLUDE_DIR"
    #   export LD_LIB_PATH="$DEVSHELL_DIR/lib:$GMP_LIB_DIR"
    # '';
    interactive = '''';
  };

  packages = [
    tetroid.tetroidHaskell.haskell.packages.ghc865.cabal-install
    (tetroid.tetroidHaskell.haskell.packages.ghc865.ghcWithPackages (p: with p; [
      aeson
      aeson-pretty
      http-types
      zlib
      polysemy
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
      # tetroid
    ]))

    binutils
    pkg-config
    openssl
    openssl.dev
    gcc
    glibc
    gmp.dev
    gmp
    nixpkgs-fmt
  ];
}
