
; Generic raise function.
(define raise (curry num-call 'raise))

; This variant of raise() returns '() if it can't.
(define (raise-safe value)
 (let* (
   (lookup (apply-generic-scope-lookup numbers-scope))
   (type   (apply-generic-tag-get value))
   (raise  (lookup 'raise (list type)))
  )

  (if (not (procedure? raise)) '()
   ;—> raise implementations do take unwrapped:
   (raise (apply-generic-unwrap value))
  )
 )
)

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
