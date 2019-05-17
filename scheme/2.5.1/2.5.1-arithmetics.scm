(include "apply-generic.scm")
(include "../2.4.3/curry.scm")

; Returns symbol tagging object wrapped in a tagged pair.
; In the following exercises we will overwrite the default
; implementations of tag get and object unwrap to allow
; extended tagging based on native Lisp types.
(define (num-tag-get tagged-obj)
; (if (procedure? num-tag-get-custom)
;  (num-tag-get-custom tagged-obj)
  (apply-generic-tag-get tagged-obj)
; )
)

(define (num-unwrap tagged-obj)
; (if (procedure? num-tag-get-custom)
;  (num-unwrap-custom tagged-obj)
  (apply-generic-unwrap tagged-obj)
; )
)

; The following tagging function is not needed for apply
; generic module, but for our number packages.
(define (num-tag-set tag obj)
; (if (procedure? num-tag-set-custom)
;  (num-tag-set-custom tag obj)
  (apply-generic-tag tag obj)
; )
)

; Global apply-generic scope for the numbers [only].
(define num-generic-scope
 (apply-generic-make num-tag-get num-unwrap)
)


; Generic arithmetics functions.
(define (make-num type . args)
 (define lookup (apply-generic-scope-lookup num-generic-scope))
 (apply (lookup 'num (list type)) args)
)

(define (add x y)
 ((apply-generic-scope-function num-generic-scope) 'add x y)
)

(define (sub x y)
 ((apply-generic-scope-function num-generic-scope) 'sub x y)
)

(define (mul x y)
 ((apply-generic-scope-function num-generic-scope) 'mul x y)
)

(define (div x y)
 ((apply-generic-scope-function num-generic-scope) 'div x y)
)

(define (num->str num)
 ((apply-generic-scope-function num-generic-scope) 'str num)
)


; Include arithmetics modules...
(include "2.5.1-rat.scm")
(install-rational-package num-generic-scope)
