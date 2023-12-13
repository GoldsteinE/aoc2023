#!/bin/sh

set -e
cd "$(dirname "$0")"/..

raku code/main.raku "${2}" < in/"${1}${2}.txt"
