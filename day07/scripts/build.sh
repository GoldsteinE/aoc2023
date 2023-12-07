#!/bin/sh

set -e
cd "$(dirname "$0")"/../code

# It's possible to compile Prolog, but that actually makes overall performance worse,
# since runtime is basically the same and compilation takes ~0.5s.
