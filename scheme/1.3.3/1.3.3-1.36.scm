(define (stdout . args) (for-each display args) (newline))

; (dump f Xi) gives Xi+1, (close? i Xi (dump f Xi))
(define (fixed-point x0 dump close? f)
 (define (next i xi)
  (stdout "X" i " = " xi)
  (let ((fi (dump f xi)))
   (if (close? i xi fi) fi
    (next (+ i 1) fi)
   )
  )
 )

 (next 0 x0)
)

(define (close? i xi fx) (< (abs (- xi fx)) 0.0001))

(define (test-fixed-point what-f x0 dump f)
 (stdout "f(x) = x  of  f(x) = " what-f " from x0 = " x0
  " to xN = " (fixed-point x0 dump close? f))
)

(define (dump-no f x) (f x))
(define (dump-avg f x) (* 0.5 (+ x (f x))))

(test-fixed-point "ln1000 / lnX {dump plain}" 2.0 dump-no
 (lambda (x) (/ (log 1000) (log x))))

(test-fixed-point "ln1000 / lnX {dump avg}" 2.0 dump-avg
 (lambda (x) (/ (log 1000) (log x))))
