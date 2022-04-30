#lang racket

(require racket/runtime-path racket/cmdline
         (only-in rosette clear-state! term-cache))

(define-runtime-path root ".")
(define-namespace-anchor top-ns)


; map from benchmark names to relative file paths
(define BENCHMARKS
  (hash "nanoscala"   "bonsai/nanoscala.rkt"
        "letpoly"     "bonsai/let-poly.rkt"
        "cosette"     "cosette/cidr-benchmarks/oracle-12c-bug.rkt"
        "cosette-1"   "cosette-optimized/cidr-benchmarks/oracle-12c-bug.rkt"
        "ferrite"     "ferrite/rename.rkt"
        "fluidics"    "fluidics/ex2.rkt"
        "ifcl"        "ifcl/test.rkt"))

; groups of benchmarks
(define GROUPS
  (hash "fast" (sort (set->list (set-subtract (list->set (hash-keys BENCHMARKS)) (set "greenthumb" "rtr"))) string<?)
        "sympro" (sort (set->list (set-subtract (list->set (hash-keys BENCHMARKS)) (set "frpsynth" "swizzle"))) string<?)))


; Run a benchmark at a given relative path.
; Returns 3 values -- (cpu, real, gc) time
(define (run-file relative-path)
  (with-output-to-file "/tmp/term-log.txt" #:exists 'replace void)
  (define module-path `(file ,(path->string (build-path root relative-path))))
  ; run in a fresh namespace so that we can run the same file multiple times.
  ; however, we must copy our own namespace's instantiation of rosette into
  ; the fresh namespace so that profiling parameters are preserved.
  (define ns (make-base-namespace))
  (namespace-attach-module (namespace-anchor->namespace top-ns) 'rosette ns)
  (parameterize ([current-namespace ns])
    ; first visit the module without instantiating,
    ; to get it compiled
    (dynamic-require module-path (void))
    ; now time the module instantiation
    (define-values (_ cpu real gc)
      (time-apply (thunk (dynamic-require module-path #f)) '()))

    (define the-term-count
      (apply + (hash-count (term-cache)) (map string->number (file->lines "/tmp/term-log.txt"))))
    (clear-state!)
    (values cpu real gc the-term-count)))


; Run the benchmark with the given name,
; printing our progress and results.
; Compute mean and confidence interval if iters > 1.
(define (run-benchmark bm #:csv? csv? #:verbose? verbose? #:iters iters)
  (if csv?
      (let ()
        (printf "~a," bm)
        (flush-output))
      (printf "=== ~a ===\n" bm))
  (define-values (cpu real solv term-count)
    (for/fold ([cpus '()][reals '()][solvs '()][term-counts '()]) ([i (in-range iters)])
      (define-values (cpu real _ the-term-count)
        (parameterize ([current-output-port (if verbose? (current-output-port) (open-output-nowhere))])
          (run-file (hash-ref BENCHMARKS bm))))
      (define solv (- real cpu))
      (if csv?
          (printf "~a,~a,~a,~a\n" cpu real solv the-term-count)
          (printf "cpu time: ~v real time: ~v solv time: ~v term count: ~v\n" cpu real solv the-term-count))
      (values (cons cpu cpus) (cons real reals) (cons solv solvs) (cons the-term-count term-counts))))
  (when (not csv?)
    (printf "\n"))
  (when (and (not csv?) (> iters 1))
    (define (mean+ci lst)
      (define t-scores
        '(0 12.71 4.303 3.182 2.776 2.571 2.447 2.365 2.306 2.262))
      (define μ (exact->inexact (/ (apply + lst) (length lst))))
      (define σ (sqrt (exact->inexact (/ (apply + (for/list ([x lst]) (expt (- x μ) 2))) (- (length lst) 1)))))
      (define δ (* (list-ref t-scores (min (- (length lst) 1) (- (length t-scores) 1))) (/ σ (sqrt (length lst)))))
      (define (fmt x) (~r x #:precision 1))
      (format "~a [~a, ~a]" (fmt μ) (fmt (- μ δ)) (fmt (+ μ δ))))
    (printf "cpu time: ~a\n" (mean+ci cpu))
    (printf "real time: ~a\n" (mean+ci real))
    (printf "solv time: ~a\n" (mean+ci solv))
    (printf "term count: ~a\n" (mean+ci term-count))
    (printf "\n")))


(module+ main
  (define csv? #f)
  (define verbose? #f)
  (define iters 1)
  (define bms
    (parse-command-line
     "run.rkt"
     (current-command-line-arguments)
     (list
      (list 'once-each
            (list '("-c" "--csv")
                  (lambda (flag) (set! csv? #t))
                  (list "Produce CSV output"))
            (list '("-v" "--verbose")
                  (lambda (flag) (set! verbose? #t))
                  (list "Report output from benchmarks"))
            (list '("-n" "--num-iters")
                  (lambda (flag n) (set! iters (string->number n)))
                  (list "Number of trials (default 1)" "n")))
      (append
       (list 'usage-help
              "where each <bm> is one of:"
              " * all (all benchmarks below)")
       (for/list ([s (sort (hash-keys GROUPS) string<?)]) (format " * ~a (~a)" s (string-join (hash-ref GROUPS s) ",")))
       (for/list ([s (sort (hash-keys BENCHMARKS) string<?)]) (format " * ~a" s))
       (list "prefix a <bm> with ~ to remove it from the set"
             "e.g. `all ~greenthumb` to run all except greenthumb")))
     (lambda (accum bm . bms)
       (for/fold ([bms '()]) ([s (cons bm bms)])
         (define spec (string-downcase s))
         (cond
           [(equal? spec "all")
            (sort (hash-keys BENCHMARKS) string<?)]
           [(equal? (string-ref spec 0) #\~)
            (define spec* (substring spec 1))
            (cond [(equal? spec* "all") '()]
                  [(hash-has-key? GROUPS spec*)
                   (for/fold ([bms bms]) ([s (hash-ref GROUPS spec*)]) (remove s bms))]
                  [(hash-has-key? BENCHMARKS spec*)
                   (remove spec* bms)]
                  [else (error "unknown benchmarks" s)])]
           [(hash-has-key? GROUPS spec)
            (append bms (hash-ref GROUPS spec))]
           [(hash-has-key? BENCHMARKS spec)
            (append bms (list spec))]
           [else
            (error "unknown benchmark" s)])))
     (list "bm" "bm")))

  (if csv?
      (printf "subject,eval,total,solv,term\n")
      (void))

  (for ([bm (remove-duplicates bms)])
    (run-benchmark bm #:csv? csv?
                      #:verbose? verbose?
                      #:iters iters)))
