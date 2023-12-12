#!/bin/sh

set -e
cd "$(dirname "$0")"/..

codebase_modified="$(stat -c '%Y' build/.unison 2>/dev/null || echo 0)"
code_modified="$(stat -c '%Y' code/main.md)"

if [ "$codebase_modified" -ge "$code_modified" ]; then
	exit 0
fi

rm -rf build/.unison
ucm transcript code/main.md --save-codebase-to build/
rm -f code/main.output.md
