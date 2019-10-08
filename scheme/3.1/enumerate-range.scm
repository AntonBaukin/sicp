
(define (enumerate-range a b)
 (define (iter i res)
  (if (< i a) res (iter (- i 1) (cons i res)))
 )

 (iter b '())
)
