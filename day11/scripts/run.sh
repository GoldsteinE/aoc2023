#!/bin/sh

set -e
cd "$(dirname "$0")"/..

build/main "${1}" "${2}" < "in/${1}${2}.txt"
