#!/bin/sh

set -e
cd "$(dirname "$0")"/..

INPUT="in/${1}${2}.txt" PART="${2}" swipl -O -g main -g halt code/main.pl
