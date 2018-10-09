(define (log . args) (for-each display args) (newline))

;—> accumulates from left to right iteratively
(define (accumulate-i a b end? next initial-value combine term)
 (define (iter accum x)
  (if (end? x b) accum
   (iter (combine accum (term x)) (next x))
  )
 )

 (iter initial-value a)
)

;—> accumulates from right to left recursively
(define (accumulate-r-r2l a b end? next initial-value combine term)
 (define (recurse x)
  (if (end? x b) initial-value
   (combine (recurse (next x)) (term x))
  )
 )

 (recurse a)
)

(define (increment i) (+ i 1))

(define (identity x) x)

(define (test-accumulate-order accumulate-x)
 (define (combine accum x) (append accum (list x)))
 (accumulate-x 0 10 > increment (list) combine identity)
)

(log "order accumulate-i left-to-right: " (test-accumulate-order accumulate-i))
(log "order accumulate-r right-to-left: " (test-accumulate-order accumulate-r-r2l))

(define (sum a b next term)
 (accumulate-i a b > next 0 + term)
)

(define (sum-integers a b) (sum a b increment identity))

(define (test-sum-integers a b)
 (log "sum-integers-i " a " .. " b " = " (sum-integers a b))
)

(test-sum-integers 1 3)
(test-sum-integers 1 10)

(define (product a b next term)
 (accumulate-i a b > next 1 * term)
)

(define (factorial n) (product 2 n increment identity))

(define (test-factorial n)
 (log "" n "! = " (factorial n))
)

(test-factorial 4 )
(test-factorial 10)
