(define (log . args) (for-each display args) (newline))

(define (filtered-accumulate a b end? next initial-value combine match? term)
 (define (iter accum x)
  (if (end? x b) accum
   (if (match? x)
    (iter (combine accum (term x)) (next x))
    (iter accum (next x))
   )
  )
 )

 (iter initial-value a)
)

(define (match-any x) #t)

(define (accumulate a b end? next initial-value combine term)
 (filtered-accumulate a b end? next initial-value combine match-any term)
)

(define (increment i) (+ i 1))

(define (identity x) x)

(define (test-accumulate-order accumulate-x)
 (define (combine accum x) (append accum (list x)))
 (accumulate-x 0 10 > increment (list) combine identity)
)

;(log "order accumulate left-to-right: " (test-accumulate-order accumulate))

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

(define (sum-matching a b next match? term)
 (filtered-accumulate a b > next 0 + match? term)
)

(define (sum-integers a b) (sum-matching a b increment match-any identity))

(define (test-sum-integers a b)
 (log "sum-integers " a " .. " b " = " (sum-integers a b))
)

;(test-sum-integers 1 3)
;(test-sum-integers 1 10)

(define (sum-primes-squares a b)
 (sum-matching a b increment prime? square)
)

(define (test-sum-primes-squares a b)
 (log "sum squares of primes in [" a " .. " b "] = " (sum-primes-squares a b))
)

(test-sum-primes-squares 1 10)
(test-sum-primes-squares 1 100)

(define (product-matching a b next match? term)
 (filtered-accumulate a b > next 1 * match? term)
)

(define (factorial n) (product-matching 2 n increment match-any identity))

(define (test-factorial n)
 (log "" n "! = " (factorial n))
)

;(test-factorial 4 )
;(test-factorial 10)

(define (gr-com-div a b)
 (define (next x y);  <-- x >= y
  (if (= 0 y) x
   (gr-com-div y (remainder x y))
  )
 )

 (if (> a b) (next a b) (next b a))
)

(define (test-gr-com-div a b)
 (log "greatest common divisor of " a " and " b " = " (gr-com-div a b))
)

;(test-gr-com-div 40 206)

(define (product-of-relative-primes-till n)
 (define (relative-prime? i) (= 1 (gr-com-div i n)))
 (product-matching 2 (- n 1) increment relative-prime? identity)
)

(define (test-product-of-relative-primes-till n)
 (log "product of relative primes till " n " = " (product-of-relative-primes-till n))
)

(test-product-of-relative-primes-till 10)
