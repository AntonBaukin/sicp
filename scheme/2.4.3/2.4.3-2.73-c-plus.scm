(include "../2.3.2/2.3.2-2.5z-plus.scm")
(include "2.4.3-2.73-c.scm")

(define (deriv-diff var expr)
 (if (not (diff? expr))
  (error "Not a diff expression to take derivative" expr)
  (make-diff
   (deriv var (minuend expr))
   (deriv var (subtrahend expr))
  )
 )
)

(define (install-deriv-diff)
 (apply-generic-put 'deriv '(-) deriv-diff)
)

(define (deriv-div var expr)
 (if (not (div? expr))
  (error "Not a div expression to take derivative" expr)
  (make-div
   (make-diff
    (make-product
     (deriv var (numerator expr))
     (denominator expr)
    )

    (make-product
     (numerator expr)
     (deriv var (denominator expr))
    )
   )

   (make-exp (denominator expr) 2)
  )
 )
)

(define (install-deriv-div)
 (apply-generic-put 'deriv '(/) deriv-div)
)
