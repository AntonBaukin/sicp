(define (log . args) (for-each display args) (newline))

(include "tree-print.scm")

(define (tree-map tree mapper)
 (define (bound-tree-map item)
  (map tree-mapper item)
 )

 (define (tree-mapper item)
  (if (pair? item)
   (bound-tree-map item)
   (mapper item)
  )
 )
 
 (bound-tree-map tree)
)

(define (square-tree-mapped tree)
 (tree-map tree square)
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
 (log "tree squares via general mapping")
 (log (tree-of-numbers-str (square-tree-mapped tree)))
)

(test-square-tree (list 2 3 4 (list 5 6 (list 7 8) 9) 10))
