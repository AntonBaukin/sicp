(define (log . args) (for-each display args) (newline))

(include "tree-print.scm")

(define (square-tree-plain tree)
 (if (null? tree) tree
  (if (pair? tree)
    (cons (square-tree-plain (car tree)) (square-tree-plain (cdr tree)))
    (square tree)
   )
 )
)

(define (square-tree-mapped tree)
 (define (mapper item)
  (if (pair? item)
   (square-tree-mapped item)
   (square item)
  )
 )
 (map mapper tree)
)

(define (tree-of-numbers-str tree)
 (tree-knots-print-str tree #f (lambda (item)
  (string-append " " (number->string item))))
)

(define (test-square-tree tree)
 (newline)
 (log "tree " tree)
 (log (tree-of-numbers-str tree))

 (newline)
 (log "tree squares plain")
 (log (tree-of-numbers-str (square-tree-plain tree)))

 (newline)
 (log "tree squares mapped")
 (log (tree-of-numbers-str (square-tree-mapped tree)))
)

(test-square-tree (list 2 3 4 (list 5 6 (list 7 8) 9) 10))
