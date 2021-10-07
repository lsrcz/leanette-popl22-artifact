#lang rosette

(provide (all-defined-out))

(define target-spectre (make-parameter #f))

(define (concrete?)
  (&& (null? (asserts))
      (equal? #t (pc))))

(define assert-db (make-parameter (make-hash)))

(define (bug-clear!) (assert-db (make-hash)))

(define (bug-format data sol)
  (define key (dict-ref data 'key #f))
  (define location (dict-ref data 'location #f))
  (define message ((dict-ref data 'message (thunk* #f)) sol))
  (define (fmt v) (if v (~a v) "<unknown>"))
  (string-join (map fmt (list key location message)) ": "))

(define (bug-assert x #:key [key #f] #:dbg [dbg #f] #:msg [msg "Unknown bug-assert"])
  (define msg-proc (if (procedure? msg) msg (thunk* msg)))
  (define expr (=> (pc) x))
  (define data `((key      . ,key)
                 (location . ,dbg)
                 (message  . ,msg-proc)))
  (when (! x)
    (hash-set! (assert-db) expr (cons data (hash-ref! (assert-db) expr null))))
  ; show a concrete message if this is trivially false
  (define msg-str (bug-format data (sat)))
  (assert x msg-str))

(define (bug-on x #:key [key #f] #:dbg [dbg #f] #:msg [msg "Unknown bug-on"])
  (bug-assert (! x) #:key key #:dbg dbg #:msg msg))

(define (bug #:key [key #f] #:dbg [dbg #f] #:msg [msg "Unknown bug"])
  (bug-on #t #:key key #:dbg dbg #:msg msg))

(define (bug-ref expr #:key [key #f])
  (hash-ref (assert-db) expr null))

(define (simplify-asserts asserted)
  (filter-not (lambda (expr) (unsat? (verify (assert expr)))) asserted))

; spectre

; For spectre checks we need to hide path conditions from Rosette to
; mimic speculative execution.  Use parameter as it's not lifted by
; Rosette; this will omit path conditions when using spectre-bug-on.

(define spectre-asserts (make-parameter null))

(define (spectre-bug-on expr #:msg [msg #f] #:dbg [dbg #f])
  (when (target-spectre)
    (spectre-asserts (cons (lambda () (bug-on expr #:msg msg #:dbg dbg)) (spectre-asserts)))))

(define-syntax-rule (with-spectre-asserts expr)
  (with-asserts
    (begin0
      (begin expr)
      (for-each (lambda (f) (f)) (spectre-asserts))
      (spectre-asserts null))))

(define-syntax-rule (with-spectre-asserts-only form)
  (let-values ([(out asserts) (with-spectre-asserts form)])
    asserts))
