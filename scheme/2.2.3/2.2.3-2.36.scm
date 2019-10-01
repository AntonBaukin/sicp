(define (log . args) (for-each display args) (newline))

(define (accumulate sequence initial op)
 (define (recurse tail)
  (if (null? tail) initial
   (op (car tail) (recurse (cdr tail)))
  )
 )

 (recurse sequence)
)

(define (accumulate-n sequences initial op)
 (if (null? (car sequences)) (list)
  (cons
   (accumulate (map (lambda (l) (car l)) sequences) initial op)
   (accumulate-n (map (lambda (l) (cdr l)) sequences) initial op)
  )
 )
)

(define matrix (list
 (list  1  2  3)
 (list  4  5  6)
 (list  7  8  9)
 (list 10 11 12)
))

(log "sum of matrix rows " matrix " = " (accumulate-n matrix 0 +))
