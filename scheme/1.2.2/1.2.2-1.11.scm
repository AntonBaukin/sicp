; f(n) = n, if n < 3, else f(n) = f(n - 1) + f(n - 2) + f(n - 3)

(define (log . args) (for-each display args) (newline))

(define (fr n) (if (< n 3) n
  (+ (fr (- n 1)) (fr (- n 2)) (fr (- n 3))))
)

(define (fi n)
 (define (next i f1 f2 f3) (if (= i 2) f1
   (next (- i 1) (+ f1 f2 f3) f1 f2))
 )

 (if (< n 3) n (next n 2 1 0))
)

(log "fr(5) = " (fr 5))
(log "fi(5) = " (fi 5))

(newline)

(log "fr(11) = " (fr 11))
(log "fi(11) = " (fi 11))
