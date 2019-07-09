(include "2.5.2-tower.scm")

; Generic raise function.
(define raise (curry num-call 'raise))

; Register 3 raise implementations.
((apply-generic-scope-register numbers-scope)

 ; integer —> rational
 'raise '(integer) (lambda (i)
  (make-rat i 1)
 )

 ; rational —> float number
 'raise '(rational) (lambda (r)
  (make-number (/ (* 1.0 (car r)) (cdr r)))
 )

 ; float number —> complex
 'raise '(number) (lambda (n)
  (make-complex-xy n 0)
 )
)
