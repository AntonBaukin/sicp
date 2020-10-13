
(define (test . vals)
 (define (next vals)
  (if (not (null? vals))
   (begin
    (assert-eq? (car vals) (eval-basic (ramb '(1 2 3 4 5))))
    (next (cdr vals))
   )
  )
 )

 (next vals)
)

(eval-basic (ramb-seed 1000033))
(test 4 2 2 4 1)
