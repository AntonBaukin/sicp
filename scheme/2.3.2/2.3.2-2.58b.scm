(include "2.3.2-2.5y.scm")
(define (log . args) (for-each display args) (newline))

(define (make-sum-expr expr-a expr-b)
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

(define (make-sum expr-a expr-b)
 (cond

  ((sum? expr-a)
   (append expr-a (list '+ expr-b))
  )

  ((sum? expr-b)
   (append (list expr-a '+) expr-b)
  )

  (else (make-sum-expr expr-a expr-b))
 )
)

(define (addend sum-expr)
 (car sum-expr)
)

(define (augend sum-expr)
 (let ((res (cddr sum-expr)))
  (if (null? (cdr res)) (car res) res)
 )
)

(define (make-product-expr expr-a expr-b)
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

(define (make-product expr-a expr-b)
 (cond

  ((product? expr-a)
   (append expr-a (list '* expr-b))
  )

  ((product? expr-b)
   (append (list expr-a '*) expr-b)
  )

  (else (make-product-expr expr-a expr-b))
 )
)

(define (multiplier product-expr)
 (car product-expr)
)

(define (multiplicand product-expr)
 (let ((res (cddr product-expr)))
  (if (null? (cdr res)) (car res) res)
 )
)

(log "deriv (x + 2 + x + 4 + x) = " (deriv-x '(x + 2 + x + 4 + x)))
(log "deriv (x * 2 * x) = " (deriv-x '(x * 2 * x)))
;(log "deriv (x + 3 * (x + y + 2)) = " (deriv-x '(x + (3 * (x + (y + 2))))))
