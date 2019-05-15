(include "../2.4.3/apply-generic.scm")
(include "../2.4.3/curry.scm")

; In the included modules we use aliases of the
; generic functions to allow to overwrite them
; in distinct exercises.
(define (num-tag-set tag num)
 (apply-generic-tag-set tag num)
)

; For now, we call default implementation of apply.
(define (apply-generic-num op-symbol . args)
 (apply apply-generic (cons op-symbol args))
)


; Generic arithmetics functions.
(define (make-num type . args)
 (apply (apply-generic-get 'num (list type)) args)
)

(define (add x y)
 (apply-generic-num 'add x y)
)

(define (sub x y)
 (apply-generic-num 'sub x y)
)

(define (mul x y)
 (apply-generic-num 'mul x y)
)

(define (div x y)
 (apply-generic-num 'div x y)
)

(define (num->str num)
 (apply-generic-num 'str num)
)


; Include arithmetics modules...
(include "2.5.1-rat.scm")
(install-rational-package)
