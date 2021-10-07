#lang racket/base

(require
  "../../lib/tests.rkt"
  (only-in "../../arm64/spec.rkt" check-jit))

(module+ test
  (time (verify-stx-xadd "arm64-stx-xadd tests" check-jit)))
