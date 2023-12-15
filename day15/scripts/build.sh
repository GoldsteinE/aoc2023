#!/bin/sh

set -e
cd "$(dirname "$0")"/../code

nim compile --outdir:../build -d:release main.nim
