#!/bin/sh

set -e

cd "$(dirname "$0")"

if ! [ -x checker/build/checker ]; then
	cd checker/
	# Yeah, yeah, I'm a naughty boy/girl, what are you gonna do? Punish me?
	RUSTC_BOOTSTRAP=1 cargo build --quiet --out-dir ./build -Zunstable-options
	cd ..
fi

checker/build/checker "$@"
