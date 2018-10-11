(define (log . args) (for-each display args) (newline))

(define (smallest-divisor n)
 (define (divisor i) (= 0 (remainder n i)))
 (define (step i) (if (= i 2) 3 (+ i 2)))

 (define (next i)
  (if (> (square i) n) n
   (if (divisor i) i (next (step i)))
  )
 )

 (next 2)
)

(define (prime? n)
 (= n (smallest-divisor n))
)

(define (expmod a n m)
 (if (= n 0) 1
  (if (even? n) (remainder (square (expmod a (/ n 2) m)) m)
   (remainder (* a (expmod a (- n 1) m)) m)
  )
 )
)

(define (carmichael-num? n)
 (define (test-next a)
  (if (= a n) #t
   (if (= a (expmod a n n)) (test-next (+ a 1))
    #f
   )
  )
 )

 (if (prime? n) #f (test-next 2))
)

(define (test-carmichael-num n)
 (log "Carmichael number " (carmichael-num? n))
)

;(test-carmichael-num 561)
;(test-carmichael-num 1105)
;(test-carmichael-num 1729)

(define (search-for-carmichael-nums from to)
 (define (next l n)
  (if (= n to) l
   (if (not (carmichael-num? n)) (next l (+ n 1))
    (next (append l (list n)) (+ n 1))
   )
  )
 )

 (next (list) from)
)

(define (test-search-for-carmichael-nums from to)
 (log "Carmichael numbers from " from " to " to " are: "
  (search-for-carmichael-nums from to)
 )
)

(test-search-for-carmichael-nums 1 10000)
(test-search-for-carmichael-nums 10001 100000)
(test-search-for-carmichael-nums 100001 1000000)

;> Carmichael numbers from 1 to 10000 are:
;  (561 1105 1729 2465 2821 6601 8911)
;> Carmichael numbers from 10001 to 100000 are:
;  (10585 15841 29341 41041 46657 52633 62745 63973 75361)
;> Carmichael numbers from 100001 to 1000000 are:
;  (101101 115921 126217 162401 172081 188461 252601 278545 294409
;   314821 334153 340561 399001 410041 449065 488881 512461 530881
;   552721 656601 658801 670033 748657 825265 838201 852841 997633)