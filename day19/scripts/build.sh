#!/bin/sh

set -e
cd "$(dirname "$0")"/../code

cabal build --builddir ../build
