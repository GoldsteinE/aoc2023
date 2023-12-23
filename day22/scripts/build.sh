#!/bin/sh

set -e
cd "$(dirname "$0")"/../build

scalac ../code/main.scala
