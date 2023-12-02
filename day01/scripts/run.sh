#!/bin/sh

set -e
cd "$(dirname "$0")"/..

cbqn code/main.bqn "$2" "$(pwd)/in/${1}${2}.txt"
