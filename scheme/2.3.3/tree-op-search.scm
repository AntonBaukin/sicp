
(define (make-tree-op-search tree-ops)
 (define smaller? (tree-op-smaller? tree-ops))
 (define get-left (tree-op-left tree-ops))
 (define get-right (tree-op-right tree-ops))

 (define (search-node node item)
  (cond
   ((null? node) '())

   ((smaller? item (get node))
    (search-node (get-left node) item)
   )

   ((smaller? (get node) item)
    (search-node (get-right node) item)
   )

   (else node) ;<— found it
  )
 )

 (define (search tree item)
  (let ((node (search-node tree item)))
   (if (null? node) node (get node))
  )
 )


 (list search) ;<— compose list ready
)
