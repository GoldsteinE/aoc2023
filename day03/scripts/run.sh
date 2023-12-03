#!/bin/sh

set -e
cd "$(dirname "$0")"/..

PART="$2" INPUT="in/$1$2.txt" nvim --headless --cmd ':source code/main.vim' --cmd q 2>&1
