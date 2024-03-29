#lang rosette

(require "../util.rkt" "../sql.rkt" "../table.rkt"  "../evaluator.rkt" "../equal.rkt"  rosette/lib/synthax)

; ------- actual tables (only for test) -------

(define c-t
  (Table "t" (list "tid" "name")
   (list
    (cons (list 1 1) 1)
    (cons (list 2 2) 1)
	  (cons (list 3 3) 1)
	  (cons (list 4 4) 1)
	  (cons (list 5 5) 1))))

(define c-tr
  (Table "tr" (list "rid" "tid" "type")
         (list
          (cons (list 101 1 1) 1)
	  (cons (list 102 2 2) 1)
	  (cons (list 103 3 2) 1)
	  (cons (list 104 4 "null-symbol") 1))))

(define c-ta
  (Table "ta" (list "rid" "status")
	 (list (cons (list 101 1) 1))))
(define c-tb
  (Table "tb" (list "rid" "status")
	 (list (cons (list 103 0) 1)
               (cons (list 102 1) 1))))

; ==== counter example generated by Cosette

(define c2-t
  (Table "t" (list "tid" "name")
         (list
          (cons (list 0 0) 15))))

(define c2-tr
  (Table "tr" (list "rid" "tid" "type")
         (list
          (cons (list 0 0 0) 0))))

(define c2-ta
  (Table "ta" (list "rid" "status")
	 (list (cons (list 0 0) 4))))
(define c2-tb
  (Table "tb" (list "rid" "status")
	 (list (cons (list 0 0) 3))))

; ------------ symbolic tables ----------------

(define s-t (Table "t" (list "tid" "name") (gen-sym-schema 2 1)))
(define s-tr (Table "tr" (list "rid" "tid" "type") (gen-sym-schema 3 1)))
(define s-ta (Table "ta" (list "rid" "status") (gen-sym-schema 2 1)))
(define s-tb (Table "tb" (list "rid" "status") (gen-sym-schema 2 1)))

; ------------ count bug ----------------------
(define t s-t)
(define tr s-tr)
(define ta s-ta)
(define tb s-tb)

; ===== correct query ==== 

(define q1 
  (AS (LEFT-OUTER-JOIN (NAMED t) (NAMED tr) 0 1)
      ["t1" (list "tid1" "name" "rid" "tid2" "type")]))

(define q1-2 
  (AS (LEFT-OUTER-JOIN-2 
	(NAMED t) 
	(NAMED tr) 
	(SELECT (VALS "t0.tid1" "t0.name" "t0.rid" "t0.tid2" "t0.type")
	 FROM (AS (JOIN (NAMED t) (NAMED tr)) ["t0" (list "tid1" "name" "rid" "tid2" "type")])
	 WHERE (BINOP "t0.tid1" = "t0.tid2")))
      ["t1" (list "tid1" "name" "rid" "tid2" "type")]))

(define q2
  (LEFT-OUTER-JOIN-2 
    q1 
    (NAMED ta)
    (SELECT (VALS "t0.tid1" "t0.name" "t0.rid" "t0.tid2" "t0.type" "t0.rid2" "t0.status1")
     FROM   (AS (JOIN q1 (NAMED ta)) ["t0" (list "tid1" "name" "rid" "tid2" "type" "rid2" "status1")])
     WHERE  (AND (BINOP "t0.type" equal? 1) (BINOP "t0.rid" = "t0.rid2")))))

(define q3
  (AS
   (LEFT-OUTER-JOIN-2 
    q2
    (NAMED tb)
    (SELECT (VALS "t0.tid1" "t0.name" "t0.rid" "t0.tid2" "t0.type" "t0.rid2" "t0.status1" "t0.rid3" "t0.status2")
     FROM   (AS (JOIN q2 (NAMED tb)) ["t0" (list "tid1" "name" "rid" "tid2" "type" "rid2" "status1" "rid3" "status2")])
     WHERE  (AND (BINOP "t0.type" equal? 2) (BINOP "t0.rid" = "t0.rid3"))))
   ["t0" (list "tid1" "name" "rid" "tid2" "type" "rid2" "status1" "rid3" "status2")]))

(define q4
  (SELECT (VALS "t0.tid1" "t0.name" "t0.status1" "t0.status2")
          FROM q3
          WHERE (TRUE)))

; === wrong query ===

(define q1-r
 (SELECT (VALS "t0.tid1" "t0.name" "t0.rid" "t0.tid2" "t0.type")
	 FROM (AS (JOIN (NAMED t) (NAMED tr)) ["t0" (list "tid1" "name" "rid" "tid2" "type")])
	 WHERE (BINOP "t0.tid1" = "t0.tid2")))
     
(define q2-r
  (LEFT-OUTER-JOIN-2 
    q1-r
    (NAMED ta)
    (SELECT (VALS "t0.tid1" "t0.name" "t0.rid" "t0.tid2" "t0.type" "t0.rid2" "t0.status1")
     FROM   (AS (JOIN q1-r (NAMED ta)) ["t0" (list "tid1" "name" "rid" "tid2" "type" "rid2" "status1")])
     WHERE  (AND (BINOP "t0.rid" (lambda (x y) (not (equal? x y))) sqlnull) (AND (BINOP "t0.type" equal? 1) (BINOP "t0.rid" = "t0.rid2"))))))

(define q3-r
  (AS
   (LEFT-OUTER-JOIN-2 
    q2-r
    (NAMED tb)
    (SELECT (VALS "t0.tid1" "t0.name" "t0.rid" "t0.tid2" "t0.type" "t0.rid2" "t0.status1" "t0.rid3" "t0.status2")
     FROM   (AS (JOIN q2-r (NAMED tb)) ["t0" (list "tid1" "name" "rid" "tid2" "type" "rid2" "status1" "rid3" "status2")])
     WHERE  (AND (BINOP "t0.rid" (lambda (x y) (not (equal? x y))) sqlnull) (AND (BINOP "t0.type" equal? 2) (BINOP "t0.rid" = "t0.rid3")))))
   ["t0" (list "tid1" "name" "rid" "tid2" "type" "rid2" "status1" "rid3" "status2")]))

(define q4-r
  (SELECT (VALS "t0.tid1" "t0.name" "t0.status1" "t0.status2")
          FROM q3-r
          WHERE (TRUE)))

; expect model
; (run count-bug-q1)
; (run q1)
;(run q1-2)
;(remove-zero (get-content (run q4)))
;(remove-zero (get-content (run q4-r)))
; (solve count-bug-q2)
(time (verify (same q4 q4-r)))



