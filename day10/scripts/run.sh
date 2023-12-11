#!/bin/sh

set -e
cd "$(dirname "$0")"/..

php code/main.php "${2}" < in/"${1}${2}.txt"
