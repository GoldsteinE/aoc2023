#!/bin/sh

set -e
cd "$(dirname "$0")"/../build

zig build-exe -O ReleaseFast ../code/main.zig
