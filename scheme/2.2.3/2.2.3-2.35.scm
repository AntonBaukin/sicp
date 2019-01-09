(define (log . args) (for-each display args) (newline))

(define (accumulate sequence initial op)
 (define (recurse tail)
  (if (null? tail) initial
   (op (car tail) (recurse (cdr tail)))
  )
 )

 (recurse sequence)
)

(define (count-leaves tree)
 (accumulate tree 0 (lambda (item count)
  (+ count (if (pair? item) (count-leaves item) 1))
 ))
)

(define test-tree (list 1 2 3 (list 4 5 (list 6 7) 8) 9))
(log "count leaves of tree " test-tree " = " (count-leaves test-tree))
