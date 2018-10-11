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

;(test-sum-integers-r 1 3)
;(test-sum-integers-r 1 10)

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
 (log "sum-integers-r " a " .. " b " = " (sum-integers-i a b))
)

;(test-sum-integers-i 1 3)
;(test-sum-integers-i 1 10)

(define (integral a b n f)
 (define (xi i) (+ a (/ (* i (- b a)) n)))
 (define (fi i) (f (xi i)))
 (/ (* (- b a) (sum-iterative 0 (- n 1) cmp-numbers increment fi)) (* 1.0 n))
)

;(log "integral f(x) = 1 on [0; 3] = "
; (integral 0 3 1000 (lambda (x) 1)))
;
;(log "integral f(x) = x on [0; 3] = "
; (integral 0 3 1000 (lambda (x) x)))

(define (integral-simpson a b n f)
 (define (xi i) (+ a (/ (* i (- b a)) n)))
 (define (fi i)
  (if (= 0 i) (f a)
   (if (= n i) (f b)
    (* (f (xi i)) (if (odd? i) 4 2))
   )
  )
 )

 (/ (* (- b a) (sum-iterative 0 n cmp-numbers increment fi)) (* 3.0 n))
)

;(log "integral-simpson f(x) = 1 on [0; 3] = "
; (integral-simpson 0 3 1000 (lambda (x) 1)))
;
;(log "integral-simpson f(x) = x on [0; 3] = "
; (integral-simpson 0 3 1000 (lambda (x) x)))

(define (cube x) (* x x x))

(log "sum integral f(x) = x^3 on [0; 1], n = 100  = "
 (integral 0 1 100 cube))

(log "int simpson  f(x) = x^3 on [0; 1], n = 100  = "
 (integral-simpson 0 1 100 cube))

(log "sum integral f(x) = x^3 on [0; 1], n = 1000 = "
 (integral 0 1 1000 cube))

(log "int simpson  f(x) = x^3 on [0; 1], n = 1000 = "
 (integral-simpson 0 1 1000 cube))
