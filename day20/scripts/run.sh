#!/bin/sh

set -e
cd "$(dirname "$0")"/..

if [ "$1" = "demo" ] && [ "$2" = "2" ]; then
    # There's no demo 2 in this task.
    # The algorithm used for the part 2 solution is not applicable to the part 1 demo,
    # and there's no answer provided in the task text anyway.
    exit 0
fi

gst code/main.st -a "$2" < in/"${1}${2}.txt"
