#!/usr/bin/env bash
set -euo pipefail

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
raco cross --workspace "$parent_path/../rosette-4" racket line-count.rkt rosette-4
raco cross --workspace "$parent_path/../rosette-3" racket line-count.rkt rosette-3
raco cross --workspace "$parent_path/../rosette-4" racket report.rkt
