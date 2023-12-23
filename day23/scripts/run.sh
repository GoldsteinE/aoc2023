#!/bin/sh

set -e
cd "$(dirname "$0")"/..

racket -f code/main.rkt -- "$1" "$2"
