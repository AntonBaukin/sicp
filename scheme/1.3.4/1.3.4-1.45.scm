(define (log . args) (for-each display args) (newline))

(define precision 0.0001)

(define (fixed-point maxi x0 f)
 (define (close? i xi fx)
  (or
   (< (abs (- xi fx)) precision)
   (>= i maxi)
  )
 )

 (define (next i xi)
  (log "fixed point @" i " " xi)
  (let ((fi (f xi)))
   (if (close? i xi fi) fi
    (next (+ i 1) fi)
   )
  )
 )

 (next 0 x0)
)

(define (test-wrong-root2 a)
 (log "result of wrong 2-root (" a ") = "
  (fixed-point 5 1.0 (lambda (x) (/ a x))))
)

(define (dump-avg f)
 (lambda (x) (* 0.5 (+ x (f x))))
)

(define (test-dumped-root2 a)
 (log "result of 1-dumped 2-root (" a ") = "
  (fixed-point 1000 1.0 (dump-avg (lambda (x) (/ a x))))
 )
)

(define (test-dumped-root3 a)
 (log "result of 1-dumped 3-root (" a ") = "
  (fixed-point 1000 1.0 (dump-avg (lambda (x) (/ a (* x x)))))
 )
)

(define (repeated n f)
 (define (iter i v)
  (if (= i 0) v
   (iter (- i 1) (f v)))
 )

 (lambda (x) (iter n x))
)

(define (dump-avg-n n f)
 ((repeated n dump-avg) f)
)

(define (test-dumped2-root3 a)
 (log "result of 2-dumped 3-root (" a ") = "
  (fixed-point 1000 1.0 (dump-avg-n 2 (lambda (x) (/ a (* x x)))))
 )
)

(define (test-dumped-root4 a)
 (log "result of 1-dumped 4-root (" a ") = "
  (fixed-point 1000 1.0 (dump-avg (lambda (x) (/ a (* x x x)))))
 )
)

(define (test-dumped3-root4 a)
 (log "result of 3-dumped 4-root (" a ") = "
  (fixed-point 1000 1.0 (dump-avg-n 3 (lambda (x) (/ a (* x x x)))))
 )
)

(define (pow v n)
 (cond
  ((= n 0) 1)
  ((even? n) (square (pow v (/ n 2))))
  (else (* v (pow v (- n 1))))
 )
)

(define (test-proper-dumped-root-n n k a)
 (log "result of " k "-dumped " n "-root (" a ") = "
  (fixed-point 1000 1.0 (dump-avg-n k (lambda (x) (/ a (pow x (- n 1))))))
 )
)

(test-wrong-root2 4.0)
(test-dumped-root2 4.0)
(test-dumped-root3 8.0)
(test-dumped2-root3 8.0)
(test-dumped-root4 16.0)
(test-dumped3-root4 16.0)
(test-proper-dumped-root-n 5 2 (pow 3 5))
(test-proper-dumped-root-n 10 3 (pow 3 10))
(test-proper-dumped-root-n 20 4 (pow 3 20))
(test-proper-dumped-root-n 30 4 (pow 3 30))
