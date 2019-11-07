; Depends on «curry.scm».
; Depends on «tree-util-walk.scm».
; Depends on «tree-util-max.scm».

; Creates function that takes binary tree and
; returns value of the previous node: it's
; value is largest within smaller nodes.
;
; Note that item (value) may absent in the tree.
;
; Arguments: (tree-node item-value).
; Optional utilities: (walker get-max).
;
; Walker and get-max utility for the same tree
; ops may be given for reuse purposes.
; They are created on demand.
;
(define (make-tree-get-prev tree-ops . utils)
 (define get (tree-op-get tree-ops))
 (define get-left (tree-op-left tree-ops))
 (define smaller? (tree-op-smaller? tree-ops))

 (define walker
  (if (<= 1 (length utils))
   (list-ref utils 0)
   (make-tree-util-walker tree-ops)
  )
 )

 (define get-max
  (if (= 2 (length utils))
   (list-ref utils 1)
   (make-tree-get-max tree-ops walker)
  )
 )

 ; When breaked, the stack contains values smaller and greater
 ; than the given item. Here we find the maximum of the values
 ; that are smaller than the target item.
 (define (max-smaller item stack res)
  (cond
   ((null? stack) res)

   ((smaller? (get (car stack)) item)
    (max-smaller item (cdr stack)
     (cond
      ((null? res) (get (car stack)))
      ((smaller? res (get (car stack))) (get (car stack)))
      (else res)
     )
    )
   )

   (else (max-smaller item (cdr stack) res))
  )
 )

 (define (result item r)
  (if (and (eq? 'result (car r)) (not (null? (cadr r))))
   (cadr r)
   (max-smaller
    item
    (cons ;<— we add curent node to the list as target item
     (list-ref r 2) ; may be not found in the tree.
     (list-ref r 4)
    )
    '()
   )
  )
 )

 (define (get-max-safe tree-node)
  (if (null? tree-node) '() (get-max tree-node))
 )

 (define (navigator item node from stack)
  (cond
   ((smaller? item (get node)) 'left)
   ((smaller? (get node) item) 'right)

   ; Node's item equals to the item:
   (else (get-max-safe (get-left node)))
  )
 )

 (lambda (tree-node item)
  (result item (walker tree-node (curry navigator item)))
 )
)
