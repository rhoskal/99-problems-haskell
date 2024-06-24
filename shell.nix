let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };

  haskellDeps = ps: with ps; [ base hspec ];

  ghc = pkgs.haskell.compiler.ghc98 haskellDeps;

  inputs = [
    pkgs.gcc
    pkgs.ghc
    pkgs.ghcid
    pkgs.cabal-install
    pkgs.llvm
    pkgs.nixfmt
    pkgs.ormolu
  ];

  hooks = ''
    mkdir -p .nix-stack
    export STACK_ROOT=$PWD/.nix-stack
  '';
in pkgs.stdenv.mkDerivation {
  name = "app";
  src = ./.;
  buildInputs = inputs;
  shellHook = hooks;
}
