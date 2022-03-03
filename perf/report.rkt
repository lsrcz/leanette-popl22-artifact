#lang racket

(require csv-reading
         text-table)

(define data-3 (csv->list (open-input-file "workspace/r3.csv")))
(define data-4 (csv->list (open-input-file "workspace/r4.csv")))

(define r3 (for/hash ([row data-3]) (values (first row) (rest row))))
(define r4 (for/hash ([row data-4]) (values (first row) (rest row))))

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

(define (time-fmt x)
  (if (< x 1)
      "<\\,1"
      (add-comma (~r x))))

(define (term-fmt x)
  (add-comma (~r x)))

(define replacement (hash "Greenthumb" "GreenThumb"
                          "Ifcl" "IFCL"
                          "Memsynth" "MemSynth"
                          "Rtr" "RTR"
                          "Synthcl" "SynthCL"
                          "Websynth" "WebSynth"))

(print-table
 (cons
  (list "Benchmark" "(Rosette) Total" "Eval" "Solving" "Terms" "(Rosette*) Total" "Eval" "Solving" "Terms")
  (for/list ([name all-names])
    (match-define (list (app string->number cpu-3)
                        (app string->number real-3)
                        _
                        (app string->number term-3))
      (hash-ref r3 name))
    (match-define (list (app string->number cpu-4)
                        (app string->number real-4)
                        _
                        (app string->number term-4))
      (hash-ref r4 name))
    (define name* (string-titlecase name))
    (list (hash-ref replacement name* name*)
          (time-fmt real-3)
          (time-fmt cpu-3)
          (time-fmt (- real-3 cpu-3))
          (term-fmt term-3)
          (time-fmt real-4)
          (time-fmt cpu-4)
          (time-fmt (- real-4 cpu-4))
          (term-fmt term-4)))))

(define (sum-fmt x)
  (~r x #:precision '(= 2)))

(define-values (max-real max-cpu max-solve max-term min-real min-cpu min-solve min-term geo-real geo-cpu geo-solve geo-term)
  (for/fold ([max-real -inf.0]
             [max-cpu -inf.0]
             [max-solve -inf.0]
             [max-term -inf.0]
             [min-real +inf.0]
             [min-cpu +inf.0]
             [min-solve +inf.0]
             [min-term +inf.0]
             [geo-real 1]
             [geo-cpu 1]
             [geo-solve 1]
             [geo-term 1])
            ([name all-names])
    (match-define (list (app string->number cpu-3)
                        (app string->number real-3)
                        _
                        (app string->number term-3))
      (hash-ref r3 name))
    (match-define (list (app string->number cpu-4)
                        (app string->number real-4)
                        _
                        (app string->number term-4))
      (hash-ref r4 name))
    (define diff-real (/ real-4 real-3))
    (define diff-cpu (/ cpu-4 cpu-3))
    (define diff-solve (/ (- real-4 cpu-4) (- real-3 cpu-3)))
    (define diff-term (/ term-4 term-3))

    (values (max diff-real max-real)
            (max diff-cpu max-cpu)
            (max diff-solve max-solve)
            (max diff-term max-term)
            (min diff-real min-real)
            (min diff-cpu min-cpu)
            (min diff-solve min-solve)
            (min diff-term min-term)
            (* diff-real geo-real)
            (* diff-cpu geo-cpu)
            (* diff-solve geo-solve)
            (* diff-term geo-term))))
