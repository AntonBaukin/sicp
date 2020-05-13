(define (log . args) (for-each display args) (newline))

; This is our default implementation of evaluator,
; variable resolve in-place, as Ben tells.

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

(assert-eq? 16
 (eval-basic
  (let ((a 1))
   (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b)
   )
   (f 10)
  )
 )
)
