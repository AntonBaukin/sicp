; Depends on «tree-util-walk.scm».

; Creates function that takes binary tree and
; returns value of the right-most item.
;
; Arguments: (tree-node).
; Optional utilities: (walker).
;
; Walker for the same tree-ops may be given
; for reuse purposes. It's created on demand.
;
(define (make-tree-get-max tree-ops . utils)
 (define get (tree-op-get tree-ops))

 (define (get-result r) (get (caddr r)))
 (define (move-right node from stack) 'right)

 (define walker
  (if (= 1 (length utils))
   (list-ref utils 0)
   (make-tree-util-walker tree-ops)
  )
 )

 (lambda (tree-node)
  (get-result (walker tree-node move-right))
 )
)
