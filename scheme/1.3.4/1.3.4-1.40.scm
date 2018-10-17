(define (log . args) (for-each display args) (newline))

(define precision 0.0001)

(define (fixed-point x0 f)
 (define (close? xi fx)
  (< (abs (- xi fx)) precision)
 )

 (define (next i xi)
  ;(log "fixed point @" i " " xi)
  (let ((fi (f xi)))
   (if (close? xi fi) fi
    (next (+ i 1) fi)
   )
  )
 )

 (next 0 x0)
)

(define (dump-avg f)
 (lambda (x) (* 0.5 (+ x (f x))))
)

(define (sqrt-dump-avg a)
 (fixed-point 1.0 (dump-avg (lambda (x) (/ a x))))
)

(define (derivative f)
 (define dX (* 0.1 precision))
 (lambda (x)
  (/ (- (f (+ x dX)) (f x)) dX)
 )
)

(define (test-derivative x0 whatf f)
 (log "f = " whatf ", f' (" x0 ") = " ((derivative f) x0))
)

;(test-derivative 1.0 "x^2" square)
;(test-derivative 1.0 "x^3" (lambda (x) (* x x x)))

(define (newton-transform f)
 (let ((derf (derivative f)))
  (lambda (x)
   (- x (/ (f x) (derf x)))
  )
 )
)

(define (newton-method x0 f)
 (fixed-point x0 (newton-transform f))
)

(define (sqrt-newton a)
 (newton-method 1.0 (lambda (x) (- (square x) a)))
)

(define (test-sqrt a)
 (log "sqrt-dump-avg (" a ") = " (sqrt-dump-avg a))
 (log "sqrt-newton   (" a ") = " (sqrt-newton a))
 (log "    Vs   sqrt (" a ") = " (sqrt a))
)

;(test-sqrt 100)
;(test-sqrt 16)

(define (cubic a b c)
 (lambda (x)
  (+ (* x x x) (* a x x) (* b x) c)
 )
)

(define (test-cubic x0 a b c)
 (log "x^3 + " a " * x^2 + " b " * x + " c " = 0  in  x0 ="
  (newton-method x0 (cubic a b c))
 )
)

(test-cubic 1.0 1 1 1)