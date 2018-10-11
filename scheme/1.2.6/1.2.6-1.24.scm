(define (log . args) (for-each display args) (newline))

(define (expmod a n m)
 (if (= n 0) 1
  (if (even? n) (remainder (square (expmod a (/ n 2) m)) m)
   (remainder (* a (expmod a (- n 1) m)) m)
  )
 )
)

(define (test-expmod a n m)
  (log a " ^ " n " % " m " = " (expmod a n m) "  Vs  " (remainder (expt a n) m))
)

;(test-expmod 2 4 3)
;(test-expmod 3 7 4)
;(test-expmod 5 8 13)
;(test-expmod 161 7 199)

(define (fast-prime? times n)
 (define (fermat-test n)
  (define (test a) (= a (expmod a n n)))
  (test (+ 1 (random-integer (- n 1))))
 )
 (if (= times 0) #t
  (if (not (fermat-test n)) #f
   (fast-prime? (- times 1) n)
  )
 )
)

(define (fast-prime-std? n) (fast-prime? 5 n))

(define (test-fast-prime n)
 (log "prime? " n  " : " (fast-prime-std? n))
)

;(test-fast-prime 199)
;(test-fast-prime 1999)
;(test-fast-prime 19999)

(define (search-for-primes count from)
 (define (next l i n)
  (if (= i 0) l
   (if (not (fast-prime-std? n)) (next l i (+ n 2))
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

;> three primes from 1000 = (1009 1013 1019) took 5.700588226318359e-4
;> three primes from 10000 = (10007 10009 10037) took 8.969306945800781e-4
;> three primes from 100000 = (100003 100019 100043) took .0011420249938964844
;> three primes from 1000000 = (1000003 1000033 1000037) took .0011479854583740234