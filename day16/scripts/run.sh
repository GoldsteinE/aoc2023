#!/bin/sh

set -e
cd "$(dirname "$0")"/../build

mix run -e Day16.main -- "${2}" < ../in/"${1}${2}.txt"
