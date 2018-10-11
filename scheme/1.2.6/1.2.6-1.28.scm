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

(define (expmod-miller-rabin a n m)
 (if (= n 0) 1
  (if (odd? n) (remainder (* a (expmod-miller-rabin a (- n 1) m)) m)
   (let ((half (expmod-miller-rabin a (/ n 2) m)))
    (let
     (
      (root?
       (and
        (not (= 1 half))
        (not (= (- n 1) half))
        (= 1 (remainder (square half) m))
       )
      )
     )

     (if root? 0 (remainder (square half) m))
    )
   )
  )
 )
)

(define (fast-prime-general? test times n)
 (if (= times 0) #t
  (if (not (test n)) #f
   (fast-prime? (- times 1) n)
  )
 )
)

(define (fast-prime? times n)
 (define (fermat-test n)
  (define (test a) (= a (expmod a n n)))
  (test (+ 1 (random-integer (- n 1))))
 )

 (fast-prime-general? fermat-test times n)
)

(define (fast-prime-miller-rabin? times n)
 (define (fermat-test n)
  (define (test a) (= 1 (expmod-miller-rabin a (- n 1) n)))
  (test (+ 1 (random-integer (- n 1))))
 )

 (fast-prime-general? fermat-test times n)
)

(define (fast-prime-std? n) (fast-prime? 10 n))

(define (fast-prime-miller-rabin-std? n) (fast-prime-miller-rabin? 10 n))

(define (test-fast-prime n)
 (log n " is fast prime: " (fast-prime-std? n)
  " Vs real prime: " (prime? n)
  " Vs miller-rabin fast prime " (fast-prime-miller-rabin-std? n)
 )
)

(test-fast-prime 199)
(test-fast-prime 1999)
(test-fast-prime 19999)

(test-fast-prime 561)
(test-fast-prime 10585)
(test-fast-prime 997633)
