{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, ... }: flake-utils.lib.eachDefaultSystem (system:
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
          gcc
          # Day 1
          cbqn
        ];
      };
    }
  );
}
