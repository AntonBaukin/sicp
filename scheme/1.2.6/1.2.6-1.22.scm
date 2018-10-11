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

(define (prime? n) 
 (= n (smallest-divisor n))
)

(define (test-prime n)
 (log "prime? " n  " : " (prime? n))
)

;(test-prime 199)
;(test-prime 1999)
;(test-prime 19999)

(define (search-for-primes count from)
 (define (next l i n)
  (if (= i 0) l
   (if (not (prime? n)) (next l i (+ n 2))
    (next (append l (list n)) (- i 1) (+ n 2))
   )
  )
 )

 (if (even? from) (next (list) count (+ from 1)) (next (list) count from))
)

(define (test-search-for-primes from)
 (define (test-time ts)
  (log "three primes from " from " = " (search-for-primes 3 from)
   " took " (- (real-time) ts))
 )

 (test-time (real-time))
)

(test-search-for-primes 1000)
(test-search-for-primes 10000)
(test-search-for-primes 100000)
(test-search-for-primes 1000000)

;> three primes from 1000 = (1009 1013 1019) took 1.6188621520996094e-4
;> three primes from 10000 = (10007 10009 10037) took 6.1798095703125e-4
;> three primes from 100000 = (100003 100019 100043) took .001569986343383789
;> three primes from 1000000 = (1000003 1000033 1000037) took .0035021305084228516

;> (sqrt 10)
;3.1622776601683795
;> (/ 6.1798095703125e-4 1.6188621520996094e-4)
;3.817378497790869
;> (/ .001569986343383789 6.1798095703125e-4)
;2.540509259259259
;> (/ .0035021305084228516 .001569986343383789)
;2.230675778283979
