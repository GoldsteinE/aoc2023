#!/bin/sh

set -e
cd "$(dirname "$0")"/..

jaq --arg part "${2}" -Rsf code/main.jq < "in/${1}${2}.txt"
