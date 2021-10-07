#lang rosette

(require "debug.rkt" rackunit rackunit/text-ui rosette/lib/roseunit)

(provide (all-defined-out) run-tests (all-from-out rackunit) (all-from-out rosette/lib/roseunit))

(define quickcheck-max-success (make-parameter 100))

(define-simple-check (check-unsat? sol) (unsat? sol))
(define-simple-check (check-sat? sol) (sat? sol))

(define (color-string prefix s [out (current-output-port)])
  (if (terminal-port? out)
      (string-append prefix s "\033[0m")
      s))

(define color-succ (curry color-string "\033[0;32m"))

; A version of test-case that clears all Rosette state after; executing and
; inserts an additional check that the test does not leave around unnecessary
; assertions.
(define-syntax-rule (test-case+ name body ...)
  (test-case name (begin
    (printf "~a ~v\n" (color-succ "[ RUN      ]") name)
    (define term-count 0)
    (define (proc) (with-asserts-only
      (parameterize ([current-bitwidth (current-bitwidth)]
                     [term-cache (hash-copy (term-cache))]
                     [current-solver (current-solver)]
                     [current-oracle (oracle (current-oracle))]
                     [assert-db (hash-copy (assert-db))])
        (check-asserts-only (begin body ...))
        (check-equal? (asserts) null)
        (set! term-count (hash-count (term-cache))))))
    (define-values (result cpu-time real-time gc-time) (time-apply proc null))
    (printf "~a ~v (~vms cpu) (~vms real) (~v terms)\n" (color-succ "[       OK ]") name cpu-time real-time term-count))))

(define-syntax-rule (check-asserts expr)
  (let-values ([(result asserted) (with-asserts expr)])
    (check-unsat? (verify (assert (apply && asserted))))
    result))

(define-syntax-rule (check-asserts-only expr)
  (let ([asserted (with-asserts-only expr)])
    (check-unsat? (verify (assert (apply && asserted))))
    (void)))

; random testing

; generate a random value from a symbolic expression
(define (arbitrary expr)
  (define syms (symbolics expr))
  (define sol
    (for/hash [(v syms)]
      (values v (cond
        [(boolean? v)
         (zero? (random 2))]
        [(bv? v)
         (apply concat (build-list (bitvector-size (type-of v))
                                   (lambda (x) (bv (random 2) 1))))]))))
  (evaluate expr (sat sol)))

(define-syntax-rule (quickcheck body ...)
  (let ([n (quickcheck-max-success)])
    (for ([i (in-range n)])
      body ...)
    (printf "+++ OK, passed ~a tests.\n" n)))
