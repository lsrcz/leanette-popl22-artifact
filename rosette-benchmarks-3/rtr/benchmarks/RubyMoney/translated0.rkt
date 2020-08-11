#lang rosette
(require "../../verif_libraries/ivl.rkt")

(require racket/include)(require racket/undefined)
(define USE_BV false)
(define BVSIZE 6)(include (file "../../verif_libraries/integer.rkt"))
(include (file "../../verif_libraries/hash.rkt"))
(include (file "../../verif_libraries/bool.rkt"))
(include (file "../../verif_libraries/array.rkt"))
(include (file "../../verif_libraries/float.rkt"))
(include (file "../../verif_libraries/fixnum.rkt"))
(include (file "../../verif_libraries/helper.rkt"))
(include (file "../../verif_libraries/ids.rkt"))
(include (file "../../verif_libraries/basicobject.rkt"))
(include (file "../../verif_libraries/kernel.rkt"))


;;; OBJECT STRUCT:
(struct object ([classid][objectid] [size #:auto #:mutable] [contents #:auto #:mutable] [vec #:auto #:mutable] [id #:auto #:mutable] [value #:auto #:mutable] ) #:transparent #:auto-value (void))
 
;;; ARGUMENT DEFINITIONS:
  ; Initialize symbolic inputs to method 
  ; Initialize struct self of type Money::Arithmetic
(define self
(let () (define-symbolic* classid integer?) (let ([self (object classid (new-obj-id))])
self))
)

;;; FUNCTION DEFINITIONS:
(define (Money::Arithmetic_inst_-@ self  #:block [BLK (void)])
	(let ()
	(return (let ([self (Kernel_inst_class self )][f (Integer_inst_-@ (let ([self self])(begin(define tmpname0 (int (Money::Arithmetic_inst_fractional (object-objectid self) )))tmpname0)) )][c (let ([self self])(begin  ; Initialize tmpname1 of type Bignum or Fixnum
(define tmpname1 (begin (define-symbolic* tmpname1 integer?) (int tmpname1)))
tmpname1))])(begin  ; Initialize ret of type Money::Arithmetic
(define ret
(let () (define-symbolic* classid integer?) (let ([ret (object classid (new-obj-id))])
ret))
)(assume (Integer_inst_== (let ([self ret])(begin(define tmpname2 (int (Money::Arithmetic_inst_fractional (object-objectid self) )))tmpname2)) f )) ret)))))

(define-symbolic Money::Arithmetic_inst_fractional (~> integer? integer?))
;;;RETURN VALUE:
(define out (Money::Arithmetic_inst_-@ self ))

;;;VERIFIED ASSERTION:
(verify #:assume (assert (and )) #:guarantee (assert (unless (stuck? out) (Integer_inst_== (let ([self self])(begin(define tmpname3 (int (Money::Arithmetic_inst_fractional (object-objectid self) )))tmpname3)) (Integer_inst_-@ (let ([self out])(begin(define tmpname4 (int (Money::Arithmetic_inst_fractional (object-objectid self) )))tmpname4)) ) ))))

#|
Class Name->Class ID
Hash->0
Class->1
Array->2
Fixnum->3
Bignum->3
Integer->3
Float->4
Boolean->5
Money::Arithmetic->6
|#