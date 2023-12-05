#!/bin/sh

set -e
cd "$(dirname "$0")"/..

nix-instantiate --arg filename "$(pwd)/in/${1}${2}.txt" --arg part "$2" --eval code/main.nix
