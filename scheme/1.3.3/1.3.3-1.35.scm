(define (log . args) (for-each display args) (newline))

; (close? i Xi (f Xi))
(define (fixed-point x0 close? f)
 (define (next i xi)
  (let ((fi (f xi)))
   (if (close? i xi fi) fi
    (next (+ i 1) fi)
   )
  )
 )

 (next 0 x0)
)

(define (close-n n) (lambda (i xi fi) (= i n)))

(define (test-fixed-point what-f n x0 f)
 (log "f(x) = x  of  f(x) = " what-f " from x0 = " x0
  " to x" n " = " (fixed-point x0 (close-n n) f))
)

; --> it jumps over the result
(test-fixed-point "1/x" 10 2.0 (lambda (x) (/ 1.0 x)))
(test-fixed-point "1/x" 11 2.0 (lambda (x) (/ 1.0 x)))
(test-fixed-point "1/x" 12 2.0 (lambda (x) (/ 1.0 x)))

; (dump f Xi) gives Xi+1, (close? i Xi (dump f Xi))
(define (fixed-point-dumped x0 dump close? f)
 (define (next i xi)
  (let ((fi (dump f xi)))
   (if (close? i xi fi) fi
    (next (+ i 1) fi)
   )
  )
 )

 (next 0 x0)
)

(define (test-fixed-point-dumped what-f n x0 dump f)
 (log "f(x) = x  of  f(x) = " what-f " from x0 = " x0
  " dumped to x" n " = " (fixed-point-dumped x0 dump (close-n n) f))
)

(define (dump-avg f x) (* 0.5 (+ x (f x))))

(test-fixed-point-dumped "1/x" 2 2.0 dump-avg (lambda (x) (/ 1.0 x)))
(test-fixed-point-dumped "1/x" 10 2.0 dump-avg (lambda (x) (/ 1.0 x)))

(test-fixed-point-dumped "1 + 1/x" 10 2.0 dump-avg (lambda (x) (+ 1 (/ 1.0 x))))
