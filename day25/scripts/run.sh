#!/bin/sh

set -e
cd "$(dirname "$0")"/..

# No part 2 in this task.
if [ "$2" = "2" ]; then
	exit 0
fi

build/release/day25 < "in/${1}${2}.txt"
