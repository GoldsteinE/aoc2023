#!/bin/sh

set -e
cd "$(dirname "$0")"/../code

if [ "${AOC_IN_CI:-}" = "1" ]; then
	cabal update
fi
cabal build --builddir ../build
