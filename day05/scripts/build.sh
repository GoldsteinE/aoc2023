#!/bin/sh

set -e
cd "$(dirname "$0")"/..

# ocamlopt leaves build artifacts in the source directory
cp code/main.ml build/
trap 'rm build/main.ml' EXIT INT TERM
ocamlopt -I +str str.cmxa build/main.ml -o build/main
