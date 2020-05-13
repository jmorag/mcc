let
  inherit (import <nixpkgs> { }) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    name = "nixos-unstable-2020-03-17";
    owner = "nixos";
    repo = "nixpkgs";
    rev = "a2e06fc3423c4be53181b15c28dfbe0bcf67dd73";
    sha256 = "0bjx4iq6nyhj47q5zkqsbfgng445xwprrslj1xrv56142jn8n5r9";
  };
  pkgs = import nixpkgs { inherit config; };
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
                llvm-hs = pkgs.haskell.lib.dontCheck
                  (super.callHackage "llvm-hs" "9.0.1" {
                    llvm-config = llvm_9;
                  });
              };
            };
        };
      };
    };
    allowBroken = true;
  };
  compiler = pkgs.haskell.packages."${compilerVersion}";
  pkg = compiler.developPackage {
    root = ./.;
    source-overrides = { };
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv
      (with pkgs.haskellPackages; [ cabal-install alex happy pkgs.clang_9 ]);
  };
in pkg
