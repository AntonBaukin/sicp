
; Standart coercions for the number types tower.
;
; Note that the values here must be unwrapped,
; the same as for any registered function.
; But the returned values are wrapped!
;
(put-coercion '(integer rational) (lambda (v)
 (if (integer? v)
  (make-rat v 1)
  (error "Not an integer number value" v)
 )
))

(put-coercion '(rational number) (lambda (r)
 (make-number (/ (* 1.0 (car r)) (cdr r)))
))

(put-coercion '(number complex) (lambda (v)
 (if (number? v)
  (make-complex-xy v 0)
  (error "Not a number value" v)
 )
))
