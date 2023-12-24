#!/bin/sh

set -e
cd "$(dirname "$0")"/..

python3 code/main.py "$1" "$2" < "in/${1}${2}.txt"
