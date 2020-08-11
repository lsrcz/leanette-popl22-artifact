#lang s-exp rosette
; class thing% for handling state and updates in Rosette (also a superclass for reactive-thing%)

; set debug to #t to print debugging information
(define debug #f)

(provide thing% always stay required high medium low lowest sol->exact)

; symbolic names for priorities (represented as integers here)
; This is a bit hazardous since the update method sums the priority values in
; computing the penalty -- 11 low-priority constraints would win over a medium one
; (since these differ by a factor of 10).
; We could use a factor of 100 or 1000 instead -- at some point this seems likely
; to result in numeric stability problems but I don't know when.
(define required 10000)
(define high 1000)
(define medium 100)
(define low 10)
(define lowest 1)

; macros to add an always constraint or a stay
; all of these macros take optional #:priority and #:owner arguments
; (owner is the thing that owns the constraint or stay)
; These are done in a simplistic way rather than trying to figure out how to handle optional
; keyword macro arguments.
; This is a dynamic version of always (supports re-evaluating the expression each time by wrapping it in a lambda)
(define-syntax always
  (syntax-rules ()
    ((always expr) (always expr #:owner this #:priority required))
    ((always expr #:priority p) (always expr #:owner this #:priority p))
    ((always expr #:owner c) (always expr #:owner c #:priority required))
    ((always expr #:priority p #:owner c) (always expr #:owner c #:priority p))
    ((always expr #:owner c #:priority p) (send c add-always-helper 'expr (lambda () expr) p))))
(define-syntax stay
  (syntax-rules ()
    ((stay expr) (stay expr #:owner this #:priority lowest))
    ((stay expr #:priority p) (stay expr #:owner this #:priority p))
    ((stay expr #:owner c) (stay expr #:owner c #:priority lowest))
    ((stay expr #:priority p #:owner c) (stay expr #:owner c #:priority p))
    ((stay expr #:owner c #:priority p) (send c add-stay-helper expr p))))

; function to convert any floats in a solution to an exact number (annoyingly, in solutions returned by Rosette
; non-integral solutions are rationals, but integral solutions are floats)
(define (sol->exact sol)
  (match sol
    [(model m)
     (sat (for/hash ([(k v) m])
                    (if (equal? real? (type-of k))
                        (values k (inexact->exact v))
                        (values k v))))]
    [_ sol]))


(define thing%
  (class object%
    (super-new)
    
    ; lists of always constraints and stays (separated into required and soft constraints/stays)
    ; Each list element for the soft things will be a soft-cn of some kind
    (define required-constraint-procs '()) ; procedures that create required constraints (used for always)
    (define soft-constraint-procs '())
    (define required-stays '())   ; not sure why you'd want a required stay, but for consistency here it is!
    (define soft-stays '())
    ; always-code is just a list consisting of all of the code for the always constraints
    ; (used by reactive-thing% to decide whether there are temporal constraints)
    (define always-code '())

    ; current-solution is the current solution to the constraints.  It should only include exact numbers,
    ; not inexact.  Use the function update-current-solution to ensure this rather than setting it directly.
    (define current-solution (sat))
    (define/public (get-current-solution)
      current-solution)
    (define/public (wally-evaluate expr [soln current-solution])
      (evaluate expr soln))
    (define (update-current-solution newsol)
      (set! current-solution (sol->exact newsol)))
        
    ; clear the lists of always constraints and stays, as well as the global assertion store
    (define/public (clear)
      (set! required-constraint-procs '())
      (set! soft-constraint-procs '())
      (set! required-stays '())
      (set! soft-stays '())
      (set! always-code '())
      (clear-asserts!))
    
    ; Return a solution to the global assertion store plus all constraints declared
    ; using 'always' and 'stay'.  Required always and stay constraints must be satisfied, while
    ; soft always and stay constraints should be satisfied if possible, respecting their
    ; relative priorities.  Stay constraints are considered relative to the old-soln
    ; object at the start of solving.  After finding a solution, clear the global assertion store.
    ; When we return from calling solve, the solution object that is returned holds a solution.
    ; This thing's current-solution is also updated with the new solution found by this method.
    (define/public (solve)
      (define old-soft-stay-vals (map (lambda (s) (wally-evaluate s)) (map soft-target soft-stays)))
      ; get a handle to the current solver: ok to use the solver directly because we aren't doing finitization!
      (define solver (current-solver))
      (solver-clear solver)
      ; obtain the solution to the 
      ; * required always constraints
      ; * required stays
      ; * any assertions from Rosette's global store generated by the execution of (p)s
      (send this solver-add-required solver)
      (solver-assert solver (asserts))
      (define soln (solver-check solver))
      ;  (printf "REQUIRED:\n")
      ;  (for ([c required-constraints])
      ;    (printf " ~a\n" (term->datum c)))
      ;  (printf "ASSERTS ~a\n" (map term->datum (asserts)))
      ; raise an exception if the required constraints and stays aren't satisfiable
      (unless (sat? soln)
        (error 'wally-solve "Required constrants and stays aren't satisfiable."))
      ; cn-proc-penalties and stay-penalties are lists of penalties for the soft constraints and soft stays
      (define cn-proc-penalties (map (lambda (s) (if ((soft-target s)) 0 (soft-priority s))) soft-constraint-procs))
      (define stay-penalties (map (lambda (s old)
                                    (if (equal? (soft-target s) old) 0 (soft-priority s))) soft-stays old-soft-stay-vals))
      (define total-penalty (+ (foldl + 0 cn-proc-penalties) (foldl + 0 stay-penalties)))
      (when debug
        (printf "cn-proc-penalties: ~a\n" cn-proc-penalties)
        (printf "stay-penalties: ~a\n" stay-penalties)
        (printf "total-penalty: ~a\n" total-penalty))

      (solver-minimize solver (list total-penalty)) ; ask Z3 to minimize the total-penalty objective
      (update-current-solution (solver-check solver))
      
      ; Clear the global assertion store.
      (clear-asserts!)
      ; Clear the solver state.
      (solver-clear solver)
      ; Return current-solution
      current-solution)

    ; helper method - add required constraints to the solver
    (define/public (solver-add-required solver)
      (define old-required-stay-vals (map (lambda (s) (wally-evaluate s)) required-stays))
      ; add these constraints to the solver:
      ; * required always constraints
      ; * required stays
      (solver-assert solver (for/list ([p required-constraint-procs]) (p)))
      (solver-assert solver (for/list ([x required-stays] [old old-required-stay-vals])
                           (equal? x old))))
    
    ; get the source code for always constraints (to use in deciding whether they are temporally dependent)
    (define/public (get-always-code)
      always-code)
    
    ; helper methods for adding always constraints and stays
    ; these are declared as public, but should only be used by the macros for always and stay
    (define/public (add-always-helper expr fn p)
      (if (= p required) 
          (set! required-constraint-procs (cons fn required-constraint-procs))
          (set! soft-constraint-procs (cons (soft fn p) soft-constraint-procs)))
      (set! always-code (cons expr always-code)))
    (define/public (add-stay-helper obj p)
      (if (= p required)
          (set! required-stays (cons obj required-stays))
          (set! soft-stays (cons (soft obj p) soft-stays))))
    ; struct to represent soft constraints and stays.  For internal use only.
    ; target is one of the following:
    ;   a predicate (presumably containing one or more symbolic variables) - from always
    ;   a procedure that evaluates to a predicate - from always*
    ;   an expression - from stay
    ; Which one it is is determined by which list the struct is stored in.
    (struct soft (target priority) #:transparent)
    
    ))