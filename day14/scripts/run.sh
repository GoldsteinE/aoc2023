#!/bin/sh

set -e
cd "$(dirname "$0")"/..

julia code/main.jl "${2}" < in/"${1}${2}.txt"
