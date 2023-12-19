#!/bin/sh

set -e
cd "$(dirname "$0")"/../code

$(cabal exec which day19 --builddir ../build) "${2}" < ../in/"${1}${2}.txt"
