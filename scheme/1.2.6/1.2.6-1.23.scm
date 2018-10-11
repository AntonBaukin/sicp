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

(define (smallest-divisor-2 n)
 (define (divisor i) (= 0 (remainder n i)))
 (define (step i) (if (= i 2) 3 (+ i 2)))

 (define (next i)
  (if (> (square i) n) n
   (if (divisor i) i (next (step i)))
  )
 )

 (next 2)
)

(define (test-smallest-divisor-2 n)
  (log "smallest divisor " n " = " (smallest-divisor-2 n))
)

;(test-smallest-divisor-2 199)
;(test-smallest-divisor-2 1999)
;(test-smallest-divisor-2 19999)

(define (prime? n) 
 (= n (smallest-divisor n))
)

(define (prime-2? n)
 (= n (smallest-divisor-2 n))
)

(define (test-prime-2 n)
 (log "prime-2? " n  " : " (prime-2? n))
)

;(test-prime-2 199)
;(test-prime-2 1999)
;(test-prime-2 19999)

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

(define (search-for-primes-2 count from)
 (define (next l i n)
  (if (= i 0) l
   (if (not (prime-2? n)) (next l i (+ n 2))
    (next (append l (list n)) (- i 1) (+ n 2))
   )
  )
 )

 (if (even? from) (next (list) count (+ from 1)) (next (list) count from))
)

(define (test-search-for-primes from)
 (define (test-time ts)
  (log "1) three primes from " from " = " (search-for-primes 3 from)
   " took " (- (real-time) ts))
 )

 (define (test-time-2 ts)
  (log "2) three primes from " from " = " (search-for-primes-2 3 from)
   " took " (- (real-time) ts))
 )

 (test-time (real-time))
 (test-time-2 (real-time))
)

(test-search-for-primes 1000)
(test-search-for-primes 10000)
(test-search-for-primes 100000)
(test-search-for-primes 1000000)

;> 1) three primes from 1000 = (1009 1013 1019) took 1.690387725830078e-4
;> 2) three primes from 1000 = (1009 1013 1019) took 1.1491775512695312e-4
;> 1) three primes from 10000 = (10007 10009 10037) took 6.160736083984375e-4
;> 2) three primes from 10000 = (10007 10009 10037) took 3.619194030761719e-4
;> 1) three primes from 100000 = (100003 100019 100043) took .0016739368438720703
;> 2) three primes from 100000 = (100003 100019 100043) took 8.728504180908203e-4
;> 1) three primes from 1000000 = (1000003 1000033 1000037) took .0036771297454833984
;> 2) three primes from 1000000 = (1000003 1000033 1000037) took .0019240379333496094