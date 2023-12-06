#!/bin/sh

set -e
cd "$(dirname "$0")"/..

case "$2" in
	1)
		build/main < in/"${1}${2}.txt"
		;;
	2)
		build/main - < in/"${1}${2}.txt"
		;;
	*)
		echo "Usage: $0 (part|demo) (1|2)" >&2
		exit 1
		;;
esac
