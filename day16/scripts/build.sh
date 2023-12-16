#!/bin/sh

set -e
cd "$(dirname "$0")"/../build

cp -r ../code/lib ../code/mix.exs ../code/mix.lock .
mix deps.get
mix compile
