(include "apply-generic.scm")
(include "../2.3.2/2.3.2-2.5y.scm")

(define (deriv-x expr)
 (deriv 'x expr)
)

; Alternative implementation of the derivative. It differs
; from §2.3.2 in absence of «deriv-else» argument that was
; replaced with registration of derivative handlers via
; apply-generic table dispatching.
;
; To add new handlers, instead of chaining derivative
; function itself, you simply install new handlers.
; The basic handlers are also installed.
;
; By dispatching handlers via the table we eliminate
; complexity of «deriv-outer» argument — used within
; a handler to call the most general user' derivative
; implementation. Here it's just basic «deriv»!
(define (deriv var expr)
 (cond
  ; We may not use a number predicate directly as
  ; a number is not a tagged value.
  ((number? expr) 0)

  ; Variables are symbols of the expression, thus they
  ; may not be defined in general way, and table
  ; dispatching is not used for them.
  ((variable? expr) (if (same-variable? expr var) 1 0))

  (else (deriv-dispatch var expr))
 )
)

(define (deriv-dispatch-find expr)
 (apply-generic-get 'deriv (list (operator expr)))
)

(define (deriv-dispatch var expr)
 (let (
   (df (deriv-dispatch-find expr))
  )
  (if (null? df)
   (deriv-error var expr '())
   (df var expr) ;<— full expression, not the operands only
  )
 )
)

(define (deriv-sum var expr)
 (if (not (sum? expr))
  (error "Not a sum expression to take derivative" expr)
  (make-sum
    (deriv var (addend expr))
    (deriv var (augend expr))
    )
 )
)

(define (install-deriv-sum)
 (apply-generic-put 'deriv '(+) deriv-sum)
)

(define (deriv-product var expr)
 (if (not (product? expr))
  (error "Not a product expression to take derivative" expr)
  (make-sum
   (make-product
    (multiplier expr)
    (deriv var (multiplicand expr))
   )

   (make-product
    (deriv var (multiplier expr))
    (multiplicand expr)
   )
  )
 )
)

(define (install-deriv-product)
 (apply-generic-put 'deriv '(*) deriv-product)
)
