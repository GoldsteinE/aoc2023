{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { nixpkgs, flake-utils, rust-overlay, ... }: flake-utils.lib.eachDefaultSystem (system:
    let pkgs = (import nixpkgs { inherit system; overlays = [ (import rust-overlay) ]; }); in
    {
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          gnupg
          # For checker
          (rust-bin.stable.latest.default.override {
            extensions = [
              "rust-src"
              "rust-analyzer"
            ];
          })
          # For scripts
          shellcheck
          # Generally useful
          gdb
          # Day 1
          cbqn
          # Day 2
          jaq
          # Day 3
          neovim
          # Day 5
          ocaml
          # Day 6
          gcc
          binutils
          # Day 7
          swiProlog
          # Day 8
          gmp
          gnu-cobol.bin
          # Day 9
          dash
          # Day 10
          php
          # Day 11
          gnat
          # Day 12
          unison-ucm
          util-linux
          # Day 13
          rakudo
          # Day 14
          julia
          # Day 15
          nim
          # Day 16
          elixir
          # Day 17
          zig
          # Day 18
          factor-lang
          # Day 19
          ghc
          cabal-install
          # Day 20
          gnu-smalltalk
          # Day 21
          boost.dev
          # Day 22
          scala_3
          # Day 23
          racket
        ];
        shellHook = ''
          # Why did you name your language the same as one of the coreutils.
          export PATH="${pkgs.factor-lang}/bin:$PATH"
          # To work with Factor code.
          export FACTOR_ROOTS="$(pwd)/day18/code";
        '';
        # For scripts.
        FACTOR_BIN = "${pkgs.factor-lang}/bin/factor";
      };
    }
  );
}
