#!/bin/sh

set -e
cd "$(dirname "$0")"/../build

gnat compile -gnat2020 ../code/main.adb
gnat bind main
gnat link main
