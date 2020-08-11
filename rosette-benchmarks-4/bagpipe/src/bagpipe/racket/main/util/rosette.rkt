#lang s-exp rosette

(require
  racket/hash
  (only-in rosette terms constant? model sat)
  (only-in rosette/base/core/type solvable-default get-type))

(provide solve/evaluate/concretize symbolic-void symbolic-boolean rosette-eq?)

(define concretize
  (case-lambda
    [(sol)
     (concretize sol (filter constant? (terms)))]
    [(sol consts)
     (match sol
       [(model bindings)
        (sat
         (hash-union
          bindings
          (for/hash ([c consts] #:unless (dict-has-key? bindings c))
            (values c (solvable-default (get-type c))))))]
       [_ (error 'complete "expected a sat? solution, given ~a" sol)])]))

(define-syntax-rule (solve/evaluate/concretize expr)
  (let* ([out (void)]
         [sol (solve (set! out (expr (void))))])
    (if (unsat? sol) '(Uninhabited)
    (if (unknown? sol) '(Unknown)
      `(Solution ,(evaluate out (concretize sol)))))))

(define (symbolic-void _) (assert false))

(define (((symbolic-boolean s) t) v)
  (define-symbolic* b boolean?) (if b (s v) (t v)))

(define rosette-eq? eq?)