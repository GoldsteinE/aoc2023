#!/bin/sh

set -e
cd "$(dirname "$0")"/../code

as main.asm -o ../build/main.o
ld ../build/main.o -o ../build/main
