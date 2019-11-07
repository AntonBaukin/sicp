; Depends on «curry.scm».
; Depends on «tree-util-walk.scm».
; Depends on «tree-util-min.scm».

; Creates function that takes binary tree and
; returns value of the next node: it's value
; is smallest within greater nodes.
;
; Note that item (value) may absent in the tree.
;
; Arguments: (tree-node item-value).
; Optional utilities: (walker get-min).
;
; Walker and get-min utility for the same tree
; ops may be given for reuse purposes.
; They are created on demand.
;
(define (make-tree-get-next tree-ops . utils)
 (define get (tree-op-get tree-ops))
 (define get-right (tree-op-right tree-ops))
 (define smaller? (tree-op-smaller? tree-ops))

 (define walker
  (if (<= 1 (length utils))
   (list-ref utils 0)
   (make-tree-util-walker tree-ops)
  )
 )

 (define get-min
  (if (= 2 (length utils))
   (list-ref utils 1)
   (make-tree-get-min tree-ops)
  )
 )

 ; When breaked, the stack contains values smaller and greater
 ; than the given item. Here we find the minimum of the values
 ; that are greater than the target item.
 (define (min-greater item stack res)
  (cond
   ((null? stack) res)

   ((smaller? item (get (car stack)))
    (min-greater item (cdr stack)
     (cond
      ((null? res) (get (car stack)))
      ((smaller? (get (car stack)) res) (get (car stack)))
      (else res)
     )
    )
   )

   (else (min-greater item (cdr stack) res))
  )
 )

 (define (result item r)
  (if (and (eq? 'result (car r)) (not (null? (cadr r))))
   (cadr r)
   (min-greater
    item
    (cons ;<— we add curent node to the list as target item
     (list-ref r 2) ; may be not found in the tree.
     (list-ref r 4)
    )
    '()
   )
  )
 )

 (define (get-min-safe tree-node)
  (if (null? tree-node) '() (get-min tree-node))
 )

 (define (navigator item node from stack)
  (cond
   ((smaller? item (get node)) 'left)
   ((smaller? (get node) item) 'right)

   ; Node's item equals to the item:
   (else (get-min-safe (get-right node)))
  )
 )

 (lambda (tree-node item)
  (result item (walker tree-node (curry navigator item)))
 )
)
