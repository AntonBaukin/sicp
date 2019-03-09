(include "2.4.3-2.73-c-plus.scm")

; Here we swap the arguments of the table lookup.
; We have to alter every registrator of the handlers!
(define (deriv-dispatch-find expr)
 (apply-generic-get (operator expr) '(deriv))
)

(define (install-deriv-sum)
 (apply-generic-put '+ '(deriv) deriv-sum)
)

(define (install-deriv-product)
 (apply-generic-put '* '(deriv) deriv-product)
)

(define (install-deriv-exp)
 (apply-generic-put '^ '(deriv) deriv-exp)
)

(define (install-deriv-diff)
 (apply-generic-put '- '(deriv) deriv-diff)
)

(define (install-deriv-div)
 (apply-generic-put '/ '(deriv) deriv-div)
)
