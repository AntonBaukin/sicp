(include "../2.3.2/2.3.2-2.5z.scm")
(include "2.4.3-2.73-ab.scm")

(define (deriv-exp var expr)
 (if (not (exponentation? expr))
  (error "Not an exp expression to take derivative" expr)
  (make-product
   (make-product
    (exponent expr)
    (make-exp (base expr) (- (exponent expr) 1))
   )

   (deriv var (base expr))
  )
 )
)

(define (install-deriv-exp)
 (apply-generic-put 'deriv '(^) deriv-exp)
)
