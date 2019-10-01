(include "2.3.2-2.5y.scm")
(define (log . args) (for-each display args) (newline))

(define (make-sum expr-a expr-b)
 (cond
  ((=number? 0 expr-a) expr-b)
  ((=number? 0 expr-b) expr-a)

  ((and (number? expr-a) (number? expr-b))
   (+ expr-a expr-b)
  )

  (else (list expr-a '+ expr-b))
 )
)

(define (sum? expr)
 (and (pair? expr) (eq? '+ (cadr expr)))
)

(define (addend sum-expr)
 (car sum-expr)
)

(define (make-product expr-a expr-b)
 (cond
  ((=number? 0 expr-a) 0)
  ((=number? 0 expr-b) 0)
  ((=number? 1 expr-a) expr-b)
  ((=number? 1 expr-b) expr-a)

  ((and (number? expr-a) (number? expr-b))
   (* expr-a expr-b)
  )

  (else (list expr-a '* expr-b))
 )
)

(define (product? expr)
 (and (pair? expr) (eq? '* (cadr expr)))
)

(define (multiplier product-expr)
 (car product-expr)
)

(log "deriv (x + 3) = " (deriv-x '(x + 3)))
(log "deriv (x * 3) = " (deriv-x '(x * 3)))
(log "deriv (x + (3 * (x + (y + 2)))) = " (deriv-x '(x + (3 * (x + (y + 2))))))
