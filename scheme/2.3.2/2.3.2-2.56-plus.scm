(include "2.3.2-2.56.scm")

(define (make-diff expr-a expr-b)
 (cond
  ((=number? 0 expr-a) expr-b)
  ((=number? 0 expr-b) expr-a)

  ((and (number? expr-a) (number? expr-b))
   (- expr-a expr-b)
  )

  ((equal? expr-a expr-b) 0)

  (else (list '- expr-a expr-b))
 )
)

(define (diff? expr)
 (and (pair? expr) (eq? '- (car expr)))
)

(define (minuend sum-expr)
 (cadr sum-expr)
)

(define (subtrahend sum-expr)
 (caddr sum-expr)
)

; Allows chaining of derivative computators.
(define (make-deriv-sub deriv-else)
 (lambda (var expr deriv-outer)
  (if (not (diff? expr))
   (deriv-else var expr deriv-outer)

   (make-diff
    (deriv-outer var (minuend expr))
    (deriv-outer var (subtrahend expr))
   )
  )
 )
)

(define (make-div expr-num expr-den)
 (cond
  ((=number? 1 expr-den) expr-num)
  ((=number? 0 expr-den) (error "Division by zero!"))
  ((=number? 0 expr-num) 0)

  ((and (number? expr-num) (number? expr-den))
   (/ expr-num expr-den)
  )

  (else (list '/ expr-num expr-den))
 )
)

(define (div? expr)
 (and (pair? expr) (eq? '/ (car expr)))
)

(define (numerator div-expr)
 (cadr div-expr)
)

(define (denominator div-expr)
 (caddr div-expr)
)

; Allows chaining of derivative computators.
(define (make-deriv-div deriv-else)
 (lambda (var expr deriv-outer)
  (if (not (div? expr))
   (deriv-else var expr deriv-outer)

   (make-div
    (make-diff
     (make-product
      (deriv-outer var (numerator expr))
      (denominator expr)
     )

     (make-product
      (numerator expr)
      (deriv-outer var (denominator expr))
     )
    )

    (make-exp (denominator expr) 2)
   )
  )
 )
)

; Nested (chained) lambdas builder.
(define (deriv-else-chain . else-makers)
 (define (make-nested tail)
  ;--> the last item of the chain is an error
  (if (null? tail) deriv-error
   ((car tail) ;<-- invoke the derivative calc maker
    (make-nested (cdr tail))
   )
  )
 )

 (make-nested else-makers)
)


(define (deriv-x expr)
 (deriv 'x (deriv-else-chain make-deriv-sub make-deriv-div make-deriv-exp) expr)
)

(log "deriv (- (* 2 x) 3) = " (deriv-x '(- (* 2 x) 3)))
(log "deriv (/ (+ x 1) 2) = " (deriv-x '(/ (+ x 1) 2)))
(log "deriv (/ (+ (^ x 2) 1) 2) = " (deriv-x '(/ (+ (^ x 2) 1) 2)))
(log "deriv (/ (- (^ x 2) (* 2 x)) 2) = " (deriv-x '(/ (- (^ x 2) (* 2 x)) 2)))
