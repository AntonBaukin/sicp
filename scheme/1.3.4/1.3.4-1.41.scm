(define (log . args) (for-each display args) (newline))

(define (double f)
 (lambda (x)
  (log x " x 2")
  (f (f x))
 )
)

(define (inc x)
 (log x " + 1")
 (+ x 1)
)

(log "((double inc) 5) = " ((double inc) 5))
(log "(((double double) inc) 5) = " (((double double) inc) 5))
(log "(((double (double double)) inc) 5) = " (((double (double double)) inc) 5))
