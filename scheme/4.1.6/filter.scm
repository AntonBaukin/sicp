
(define (filter sequence match?)
 (define (next tail res)
  (if (null? tail) res
   (next
    (cdr tail)
    (if (match? (car tail))
     (cons (car tail) res)
     res
    )
   )
  )
 )

 (reverse (next sequence '()))
)

(define (filter-not sequence match?)
 (filter sequence (lambda (item) (not (match? item))))
)
