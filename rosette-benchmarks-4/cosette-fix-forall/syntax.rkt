#lang rosette

(require "table.rkt")

(provide (all-defined-out))

;;; query structure

; select-args : a list of values
; from-queries : a list of tables/subqueries
; where-filter : a filter
(struct query-select (select-args from-query where-filter) #:transparent)
(struct query-join (query1 query2) #:transparent)
(struct query-named (table-ref) #:transparent)
(struct query-rename (query table-name) #:transparent)
(struct query-rename-full (query table-name column-names) #:transparent)
(struct query-left-outer-join (query1 query2 key1 key2) #:transparent)
(struct query-left-outer-join-2 (query1 query2 join-result) #:transparent)
(struct query-union-all (query1 query2))

;;; values
(struct val-const (val) #:transparent)
(struct val-column-ref (column-name) #:transparent)

;;; filters
(struct filter-binop (op val1 val2) #:transparent)
(struct filter-conj (f1 f2) #:transparent)
(struct filter-disj (f1 f2) #:transparent)
(struct filter-not (f1) #:transparent)
(struct filter-true () #:transparent)
(struct filter-false () #:transparent)
