(include "2.3.2-2.5y.scm")
(define (log . args) (for-each display args) (newline))

(define (make-exp expr-base expr-power)
 (cond
  ((=number? 1 expr-power) expr-base)
  ((=number? 0 expr-power) 1)
  ((=number? 0 expr-base) 0)
  ((=number? 1 expr-base) 1)

  ((and (number? expr-base) (number? expr-power))
   (expt expr-base expr-power)
  )

  (else (list '^ expr-base expr-power))
 )
)

(define (exponentation? expr)
 (and (pair? expr) (eq? '^ (car expr)))
)

(define (base exp-expr)
 (cadr exp-expr)
)

(define (exponent exp-expr)
 (caddr exp-expr)
)

; Allows chaining of derivatives computators.
(define (make-deriv-exp deriv-else)
 (lambda (var expr deriv-outer)
  (if (not (exponentation? expr))
   (deriv-else var expr deriv-outer)

   (make-product
    (make-product
     (exponent expr)
     (make-exp (base expr) (- (exponent expr) 1))
    )

    (deriv-outer var (base expr))
   )
  )
 )
)

(define (deriv-x expr)
 (deriv 'x (make-deriv-exp deriv-error) expr)
)

(log "deriv (+ x 3) = " (deriv-x '(+ x 3)))
(log "deriv (^ x 3) = " (deriv-x '(^ x 3)))
(log "deriv (^ (+ x 1) 3) = " (deriv-x '(^ (+ x 1) 3)))
(log "deriv (* (^ x 3) (+ x 1)) = " (deriv-x '(* (^ x 3) (+ x 1))))
