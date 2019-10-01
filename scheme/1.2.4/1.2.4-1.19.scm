(define (log . args) (for-each display args) (newline))

(define (n-do n f)
 (define (next i) (cond ((< i n) (f i) (next (+ i 1))) (else n)))
 (next 0)
)

(define (fib n)

 (define (next i a b)
  (if (= i n)
   (cons a b)
   (next (+ i 1) b (+ a b))
  )
 )

 (cond
  ((= n 0) 0)
  ((= n 1) 1)
  (else (next 1 0 1))
 )
)

(define (fib-fast n)

 (define (next i a b p q)
  ;(log "i = " i " a = " a " b = " b " p = " p " q = " q)

  (cond
   ((= i 0) b)

   ((even? i) (next (/ i 2) a b (+ (square p) (square q)) (+ (* 2 p q) (square q))))
   (else (next (- i 1) (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q))
  )
 )

 (cond
  ((= n 0) 0)
  ((= n 1) 1)
  (else (next n 1 0 0 1))
 )
)

(define (test i)
 (log "fib   " i " = " (fib i))
 (log "fib X " i " = " (fib-fast i))
)

(n-do 20 test)
(test 10000)

