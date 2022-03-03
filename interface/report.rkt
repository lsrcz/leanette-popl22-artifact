#lang racket

(require text-table
         csv-reading)

(define data-3 (csv->list (open-input-file "workspace/rosette-3-total.csv")))
(define data-4 (csv->list (open-input-file "workspace/rosette-4-total.csv")))

(define r3 (for/hash ([row data-3]) (values (first row) (second row))))
(define r4 (for/hash ([row data-4]) (values (first row) (second row))))

(define all-names (sort (map first data-3) string<?))

(define (add-comma n)
  (define s (if (string? n)
                n
                (number->string n)))
  (string-append*
   (reverse
    (for/list ([digit (reverse (string->list s))] [i (in-naturals)])
      (cond
        [(zero? i) (string digit)]
        [(zero? (modulo i 3)) (string digit #\,)]
        [else (string digit)])))))

(print-table
 (cons
  (list "Benchmark" "Rosette 3 LoC" "Rosette 4 LoC")
  (for/list ([name (in-sequences all-names #;(list "jitterbug"))])
    (define loc-3 (string->number (hash-ref r3 name)))
    (define loc-4 (string->number (hash-ref r4 name)))
    (define name* (string-titlecase name))
    (list name*
          (add-comma loc-3)
          (add-comma loc-4)))))
