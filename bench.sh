#!/bin/sh

set -eu

mkdir -p tmp
name=${1-}
stack bench --benchmark-arguments "-o tmp/bench.html -m pattern $name" && cp tmp/bench.html tmp/bench-`date "+%Y%m%d_%H%M%S"`.html
