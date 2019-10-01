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

(define (vector-dot-product v w)
 (accumulate (map * v w) 0 +)
)

(define (matrix-mul-vector m v)
 (map (lambda (row) (vector-dot-product row v)) m)
)

(define (matrix-transpose m)
 (accumulate-n m (list) cons)
)

(define (matrix-mul-matrix m n)
 (let
  ((nt (matrix-transpose n)))
  (map (lambda (row) (matrix-mul-vector nt row)) m)
 )
)

(log "(1 2 3 4) ⋅ (5 6 7 8) = "
 (vector-dot-product (list 1 2 3 4) (list 5 6 7 8))
)

(define M (list
 (list  1  2  3)
 (list  4  5  6)
 (list  7  8  9)
 (list 10 11 12)
))

(log "matrix M = " M)

(define N (list
 (list 1 4)
 (list 2 5)
 (list 3 6)
))

(log "M ⋅ (1 2 3) = "
 (matrix-mul-vector M (list 1 2 3))
)

(log "M transpose = " (matrix-transpose M))

(log "matrix N = " N)

(log "M ⋅ N = " (matrix-mul-matrix M N))
