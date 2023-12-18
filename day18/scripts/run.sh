#!/bin/sh

set -e
cd "$(dirname "$0")"/..

FACTOR_BIN="${FACTOR_BIN:-factor}"
FACTOR_ROOTS="$(pwd)/code" "$FACTOR_BIN" -part="${2}" -run=day18 < in/"${1}${2}.txt"
