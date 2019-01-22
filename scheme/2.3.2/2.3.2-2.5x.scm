(define (variable? expr)
 (symbol? expr)
)

(define (same-variable? expr-a expr-b)
 (and
   (variable? expr-a)
   (variable? expr-b)
   (eq? expr-a expr-b)
 )
)

(define (make-sum expr-a expr-b)
 (list '+ expr-a expr-b)
)

(define (sum? expr)
 (and (pair? expr) (eq? '+ (car expr)))
)

(define (addend sum-expr)
 (cadr sum-expr)
)

(define (augend sum-expr)
 (caddr sum-expr)
)

(define (make-product expr-a expr-b)
 (list '* expr-a expr-b)
)

(define (product? expr)
 (and (pair? expr) (eq? '* (car expr)))
)

(define (multiplier product-expr)
 (cadr product-expr)
)

(define (multiplicand product-expr)
 (caddr product-expr)
)

; Follows the derivative-else protocol.
(define (deriv-error var expr deriv-outer)
 (error "Unknown derivative expression by " var expr)
)

; General derivative function with extension point.
; Derivative-else arguments are (var expr deriv-outer)
; where outer derivative — the top function to invoke
; to take derivative recursively. This technique allows
; the chaining of else-functions not knowing each other.
; That top derivatie has arguments (var expr).
(define (deriv var deriv-else expr)
 (define (deriv-bound var expr)
  (deriv var deriv-else expr)
 )

 (cond
  ((number? expr) 0)

  ((variable? expr) (if (same-variable? expr var) 1 0))

  ((sum? expr)
   (make-sum
     (deriv-bound var (addend expr))
     (deriv-bound var (augend expr))
   )
  )

  ((product? expr)
   (make-sum
     (make-product
       (multiplier expr)
       (deriv-bound var (multiplicand expr))
     )

     (make-product
       (deriv-bound var (multiplier expr))
       (multiplicand expr)
     )
   )
  )

  (else (deriv-else var expr deriv-bound))
 )
)

(define (deriv-x expr)
 (deriv 'x deriv-error expr)
)
