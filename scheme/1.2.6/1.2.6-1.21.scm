(define (log . args) (for-each display args) (newline))

(define (smallest-divisor n)
 (define (divisor i) (= 0 (remainder n i)))

 (define (next i)
  (if (> (square i) n) n
   (if (divisor i) i (next (+ i 1)))
  )
 )

 (next 2)
)

(define (test n)
  (log "smallest divisor " n " = " (smallest-divisor n))
)

(test 199)
(test 1999)
(test 19999)