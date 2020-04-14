let
  compilerVersion = "ghc883";
  config = {
    packageOverrides = pkgs: rec {
      llvm_9 = (pkgs.llvm_9.override { debugVersion = true; }).overrideAttrs
        (_: { doCheck = false; });
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compilerVersion}" =
            pkgs.haskell.packages."${compilerVersion}".override {
              overrides = self: super: {
                llvm-hs-pretty =
                  pkgs.haskell.lib.dontCheck super.llvm-hs-pretty;
                llvm-hs = super.callHackage "llvm-hs" "9.0.1" {
                  llvm-config = pkgs.llvm_9;
                };
              };
            };
        };
      };
    };
    allowBroken = true;
  };
  pkgs = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-unstable-2020-03-17";
    url =
      "https://github.com/nixos/nixpkgs/archive/a2e06fc3423c4be53181b15c28dfbe0bcf67dd73.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0bjx4iq6nyhj47q5zkqsbfgng445xwprrslj1xrv56142jn8n5r9";
  }) { inherit config; };
  compiler = pkgs.haskell.packages."${compilerVersion}";
  pkg = compiler.developPackage {
    root = ./.;
    source-overrides = { };
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv
      (with pkgs.haskellPackages; [ cabal-install alex happy ]);
  };
  buildInputs = [ pkgs.llvm_9 pkgs.clang_9 ];
in pkg.overrideAttrs
(attrs: { buildInputs = attrs.buildInputs ++ buildInputs; })
