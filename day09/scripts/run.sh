#!/bin/sh

set -e
cd "$(dirname "$0")"/..

dash code/main.sh "${2}" < "in/${1}${2}.txt"
