#!/bin/sh

set -e
cd "$(dirname "$0")"/../code

g++ -DNDEBUG -O3 -std=c++23 -o ../build/main main.cpp
