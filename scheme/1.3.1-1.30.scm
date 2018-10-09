(define (log . args) (for-each display args) (newline))

(define (sum-recursive a b cmp next term)
 (if (> (cmp a b) 0) 0
  (+ (term a) (sum-recursive (next a) b cmp next term))
 )
)

(define (cmp-numbers a b) (if (< a b) -1 (if (= a b) 0 +1)))

(define (increment i) (+ i 1))

(define (identity x) x)

(define (sum-integers-r a b) (sum-recursive a b cmp-numbers increment identity))

(define (test-sum-integers-r a b)
 (log "sum-integers-r " a " .. " b " = " (sum-integers-r a b))
)

(test-sum-integers-r 1 3)
(test-sum-integers-r 1 10)

(define (sum-iterative a b cmp next term)
 (define (iter sum x)
  (if (> (cmp x b) 0) sum
   (iter (+ sum (term x)) (next x))
  )
 )

 (iter 0 a)
)

(define (sum-integers-i a b) (sum-iterative a b cmp-numbers increment identity))

(define (test-sum-integers-i a b)
 (log "sum-integers-i " a " .. " b " = " (sum-integers-i a b))
)

(test-sum-integers-i 1 3)
(test-sum-integers-i 1 10)
