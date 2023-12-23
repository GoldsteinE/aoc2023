#!/bin/sh

set -e
cd "$(dirname "$0")"/../build

scala main "$2" < ../in/"${1}${2}.txt"
