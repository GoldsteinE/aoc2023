#!/bin/sh

set -e
cd "$(dirname "$0")"/..

flock build/.keep ucm --codebase build run main -- "${2}" < in/"${1}${2}.txt"
