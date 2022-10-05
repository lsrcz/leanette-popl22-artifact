#!/usr/bin/env bash
set -euo pipefail

parent_path=$(
    cd "$(dirname "${BASH_SOURCE[0]}")"
    pwd -P
)

cd "$parent_path/../rosette-benchmarks-3"
raco cross -q --workspace "$parent_path/../rosette-3" racket run.rkt -c all >"$parent_path/r3.csv" # | tee $parent_path/workspace/r3.csv

cd "$parent_path/../rosette-benchmarks-4"
raco cross -q --workspace "$parent_path/../rosette-4" racket run.rkt -c all >"$parent_path/r4.csv" # | tee $parent_path/workspace/r4.csv

