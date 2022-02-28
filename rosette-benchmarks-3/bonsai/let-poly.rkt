#lang rosette
(output-smt #t)
(require rosette/solver/smt/boolector (only-in rosette current-solver))
(current-solver (boolector))
; Introduction
; ------------
; This is a Bonsai implementation of a language with polymorphic references and
; assignment, following Tofte 1990 and Wright-Felleisen 1994. It is known that 
; the type system of this language is unsound. 

; This Bonsai program finds a counterexample (witness) for the soundness bug 
; by synthesizing a program that passes the typechecker and goes wrong in the 
; interpreter. The synthesis takes less than 30 minutes.  

; Here is one of the found counterexamples, translated to ML:
;
;     let a = ref (fn a => a)
;     in  a := (fn a => not a) ;
;         (!a)(1)
;     end
;
; This program is similar to the counterexamples in the aforementioned papers.




; Note that one weakness of Bonsai (and of bounded model checking in general) 
; is recursive calls. Languages with the potential for unbounded recursion
; (such as this one) will not ever terminate in symbolic execution of the 
; interpeter. (The symbolic execution of the type checker terminates, of
; course.) Hence, it is necessary to bound the length of the execution in the
; interpeter.
;
; Here, we solve the problem by preventing a lambda term from performing calls 
; in its body --- this restricts any recursion, and allows symbolic execution 
; to terminate.  One can think of more advanced techniques but this one seems 
; to be a sweet spot, balancing the complexity of the language design and model  
; checker design. 
;
; It is important to note that we made this choice with prior knowledge of the
; soundness problem we were looking for. This is a kind of bias, since we
; cannot know a priori whether such a change will affect the presence of
; soundness bugs in the language (in this case it doesn't).
;
; Note also that if *no* counterexamples are found, then we can always allow a
; bounded number of recursive calls, and gradually increase the bound if
; needed. So, this limitation is not fundamental.






; The Bonsai tree data structure
(require "tree-lib.rkt")

; The syntax of our language, in BNF
;
; Note: lambda abstractions require a type annotation, which are symbolic
; (i.e. unknown) and are inferred automatically by our solver.

(define lp-stx
  '([term (((let . name) . term) . term)     ; let-binding
          true                               ; a  boolean literal
          one                                ; an integer literal
          (((:= . name) . term) . term)      ; assignment+sequencing (let-like)
          ((λ . name) . (type . easy-term))  ; lambda abstraction
          (call . (term . term))             ; lambda application
          (op-type . term)                   ; unary operations
          name                               ; variable

;         ((if . term) term . term)          ; include `if` to experiment with
                                             ; its impact on Bonsai's running
                                             ; time
    ]
    [type int           ; integer
          bool          ; boolean
          any           ; generalized type
          (ref . type)  ; reference
          (type . type) ; arrow
    ]
    [op-type
          ! ; a  boolean operation
          - ; an integer operation
          & ; reference creation
          * ; dereferencing
    ]
    [name a b c d e] ; add more names to increase the candidate search space
    [easy-term ; lambda bodies can't make calls to avoid unbounded recursion
          true
          one
          (((:= . name) . easy-term) . easy-term)
          ((λ . name) . (type . easy-term))
          (op-type . easy-term)
          name
    ]
    ))

; Register the nonterminals as enums in our encoding.
; TODO: Hide this under the Bonsai abstraction.
(nonterminals! (harvest lp-stx))

; Convenient shorthands for some commonly-used enum values
; TODO: Hide this under the Bonsai abstraction.
(define REF  (symbol->enum 'ref))
(define INT  (symbol->enum 'int))
(define BOOL (symbol->enum 'bool))
(define ANY  (symbol->enum 'any))

; The  t* > t  predicate.
(define (is-instance? t t*)
    (tree-match t*
        ; tree-match tries to match the Bonsai tree t* against a list of 
        ; patterns. Upon a match, the pattern variables denoted by _ are 
        ; bound to values and the accompanying lambda term is called, 
        ; with the pattern variables passed as arguments.

        'int
        (λ () (equal? t INT))

        'bool
        (λ () (equal? t BOOL))

        '(ref . _)
        (λ (r)
            (and (pair? t)
                 (equal? (car t) REF)
                 (is-instance? (cdr t) r)))

        'any
        (λ () #t)
        
        '(_ . _) ; arrow type
        (λ (i o)
            (and (pair? t)
                 (not (equal? (car t) REF))
                 (is-instance? (car t) i)
                 (is-instance? (cdr t) o)))

        ))
        
; Typechecker for term `t` in environment `env`
(define (type-expr t env)
    (tree-match t

        '(((let . _) . _) . _)
        (λ (name value expr)
            (define value+ (type-expr value env))
            (define env+ (table-add env name value+))
            (define t+ (type-expr expr env+))
            t+)

        'true
        (λ () BOOL)

        'one
        (λ () INT)

        '(((:= . _) . _) . _)
        (λ (x y c)
            (define x+ (type-expr x env))
            (define y+ (type-expr y env))
            (assert (pair? x+) "Assign to non-ref")
            (assert (equal? (car x+) REF) "Assign to non-ref")
            ; The next check is insufficient! It causes a soundness error.
            (assert (is-instance? y+ (cdr x+)) "Assign type mismatch")
            (type-expr c env))

        '((λ . _) . (_ . _))
        (λ (name type term)
            (assert (pair? type))
            (define i (car type))
            (assert (not (equal? i REF)))
            (define o (cdr type))
            (define t+ (type-expr term (table-add env name i)))
            (assert (is-instance? t+ o))
            type)

        '(call . (_ . _))
        (λ (fn arg)
            (define type (type-expr fn env))
            (assert (pair? type))
            (define i (car type))
            (define o (cdr type))
            (define a+ (type-expr arg env))
            (assert (is-instance? a+ i))
            o)

;       '((if . _) _ . _)
;       (λ (c t f)
;          (define c+ (type-expr c env))
;          (define t+ (type-expr t env))
;          (define f+ (type-expr f env))
;          (assert (is-instance? c+ BOOL))
;          (assert (equal? t+ f+))
;          t+)

        '(_ . _)
        (λ (op-type x)
            (define x+ (type-expr x env))
            (tree-match op-type
                '!
                (λ ()
                    (assert (is-instance? x+ BOOL) "Negating a non-bool!")
                    BOOL)
                '-
                (λ ()
                    (assert (is-instance? x+ INT) "Arithmetic on a non-int!")
                    INT)
                '&
                (λ ()
                    (cons REF x+))
                '*
                (λ ()
                    (assert (equal? (car x+) REF) "Dereferencing a non-ref!")
                    (cdr x+))))

        '_
        (λ (name)
            (define r (table-find env name))
            r)

        ))



; An interpreter for term `t` in environment `env`
(define (eval-expr t env)
    (tree-match t

        '(((let . _) . _) . _)
        (λ (name value expr)
            (define value+ (eval-expr value env))
            (define env+ (table-add env name value+))
            (define expr+ (eval-expr expr env+))
            expr+)

        'true
        (λ () #t)

        'one
        (λ () 1)

        '(((:= . _) . _) . _)
        (λ (x y c)
            (define x+ (eval-expr x env))
            (define y+ (eval-expr y env))
            (vector-set! x+ 0 y+)
            (eval-expr c env))

        '((λ . _) . (_ . _))
        (λ (name type term)
            (list name term env))

        '(call . (_ . _))
        (λ (fn arg)
           (define fn+ (eval-expr fn env))
           (define a+ (eval-expr arg env))
           (define nm (car fn+))
           (define bd (cadr fn+))
           (define e+ (caddr fn+))
           (define env+ (table-add e+ nm a+))
           (mini-eval-expr bd (table-add e+ nm a+)))

;       '((if . _) _ . _)
;       (λ (c t f)
;           (define c+ (eval-expr c env))
;           (define t+ (eval-expr t env))
;           (define f+ (eval-expr f env))
;           (assert (boolean? c+))
;           (if c+ t+ f+))


        '(_ . _)
        (λ (op-type x)
            (define x+ (eval-expr x env))
            (tree-match op-type
                ; The counterexample fails here, calling an operation with the
                ; wrong primitive type.
                '!
                (λ ()
                    (not x+))
                '-
                (λ ()
                    (- x+))
                '&
                (λ ()
                    (vector x+))
                '*
                (λ ()
                    (vector-ref x+ 0))
                ))

        '_
        (λ (name)
            (define r (table-find env name))
            r)

        ))

; Mini-evaluator (primitive recursive), for the inside of lambdas
(define (mini-eval-expr t env)
    (tree-match t
        'true
        (λ () #t)

        'one
        (λ () 1)

        '(((:= . _) . _) . _)
        (λ (x y c)
            (define x+ (mini-eval-expr x env))
            (define y+ (mini-eval-expr y env))
            (vector-set! x+ 0 y+)
            (mini-eval-expr c env))

        '((λ . _) . (_ . _))
        (λ (name type term)
            (list name term env))

        '(_ . _)
        (λ (op-type x)
            (define x+ (mini-eval-expr x env))
            (tree-match op-type
                '!
                (λ ()
                    (not x+))

                '-
                (λ ()
                    (- x+))

                '&
                (λ ()
                    (vector x+))

                '*
                (λ ()
                    (vector-ref x+ 0))))

        '_
        (λ (name)
            (define r (table-find env name))
            r)

        ))


; Now, let's do the same thing, but with a symbolic program!
(displayln "Building the symbolic tree and imposing syntax constraints...")
(define t* (time (make! lp-stx 'term 7))) ; "7" is the size of our tree
(displayln "Done.")

(displayln "Symbolically evaluating the typechecker...")
(void (time (type-expr t* '())))
(displayln "Done.")

(displayln "Symbolically evaluating the interpreter and verifying...")
(define sol (time
              (verify
                (time (eval-expr t* '())))))
(sat? sol)



; A bonus query:
; Can we find a minimal solution -- that is, the shortest program that
; witnesses a soundness bug? Yes! But it takes much longer...
;
; (displayln "Running interpreter...")
; (define-values (v a) (with-asserts (time (eval-expr t* '()))))
; (define sol
;   (time (optimize #:minimize  (list (time (count-leaves t*)))
;                   #:guarantee (time (assert (! (apply && a)))))))