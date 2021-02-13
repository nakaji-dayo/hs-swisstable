#!/bin/sh

set -eu

mkdir -p tmp
name=${1-}
stack install --ghc-options="-O2" --ghc-options="-optc -mavx2 -O2" && swisstable-bench -o tmp/bench.html -m pattern $name && cp tmp/bench.html tmp/bench-`date "+%Y%m%d_%H%M%S"`.html
