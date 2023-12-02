#!/bin/sh

set -eu

find . -name '*.asc' | while read -r input; do
	if ! [ -f "${input%.asc}" ]; then
		gpg --decrypt --batch --passphrase-file .inputs-passphrase --output "${input%.asc}" "$input"
	fi
done
