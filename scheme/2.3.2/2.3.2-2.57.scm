(include "2.3.2-2.5z.scm")
(define (log . args) (for-each display args) (newline))

(define make-sum-expr make-sum)

(define (make-sum expr-a expr-b)
 (cond

  ((and (sum? expr-a) (sum? expr-b))
   (append expr-a (cdr expr-b))
  )

  ((sum? expr-a)
   (append expr-a (list expr-b))
  )

  ((sum? expr-b)
   (append (list '+ expr-a) (cdr expr-b))
  )

  (else (make-sum-expr expr-a expr-b))
 )
)

(define (augend sum-expr)
 (if (= 3 (length sum-expr))
  (caddr sum-expr)
  (cons '+ (cddr sum-expr))
 )
)

(define make-product-expr make-product)

(define (make-product expr-a expr-b)
 (cond

  ((and (product? expr-a) (product? expr-b))
   (append expr-a (cdr expr-b))
  )

  ((product? expr-a)
   (append expr-a (list expr-b))
  )

  ((product? expr-b)
   (append (list '* expr-a) (cdr expr-b))
  )

  (else (make-product-expr expr-a expr-b))
 )
)

(define (multiplicand product-expr)
 (if (= 3 (length product-expr))
  (caddr product-expr)
  (cons '* (cddr product-expr))
 )
)

(define make-exp-expr make-exp)

(define (make-exp expr-base expr-power)
 (if (not (exponentation? expr-base))
  (make-exp-expr expr-base expr-power)

  (list '^ (base expr-base)
   (make-product (exponent expr-base) expr-power)
  )
 )
)

(log "deriv (+ x y (* x 3)) = " (deriv-x '(+ x y (* x 3))))
(log "deriv (* x y (+ x 3)) = " (deriv-x '(* x y (+ x 3))))
(log "deriv (+ (^ x 3) (^ x 2) (* x 3)) = " (deriv-x '(+ (^ x 3) (^ x 2) (* x 3))))
(log "deriv (^ (^ x 2) 3)) = " (deriv-x '(^ (^ x 2) 3)))
