#!/bin/sh

set -e
cd "$(dirname "$0")"/../code

CARGO_TARGET_DIR=../build cargo build --release
