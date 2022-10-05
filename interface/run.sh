#!/usr/bin/env bash
set -euo pipefail

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
echo "---- Rosette 4 ----"
raco cross -q --workspace "$parent_path/../rosette-4" racket line-count.rkt rosette-4
echo "---- Rosette 3 ----"
raco cross -q --workspace "$parent_path/../rosette-3" racket line-count.rkt rosette-3
