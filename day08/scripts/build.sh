#!/bin/sh

set -e
cd "$(dirname "$0")"/../code

cobc -O3 -x main.cbl -o ../build/main
