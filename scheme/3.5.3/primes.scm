
(define prime? (
 (lambda ()
  (define TIMES 7)

  (define (expmod a n m)
   (if (= n 0) 1
    (if (even? n) (remainder (square (expmod a (/ n 2) m)) m)
     (remainder (* a (expmod a (- n 1) m)) m)
    )
   )
  )

  (define (fermat-test n)
   (define a (+ 1 (random-integer (- n 1))))
   (= a (expmod a n n))
  )

  (define (fast-prime? times n)
   (if (= times 0) #t
    (if (not (fermat-test n)) #f
     (fast-prime? (- times 1) n)
    )
   )
  )

  ; Resulting prime? predicate:
  (lambda (n)
   (if (= 1 n) #t (fast-prime? TIMES n))
  )
 )
))
