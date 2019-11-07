
; Creates function that takes binary tree and
; returns value of the left-most item.
; Arguments: (tree-node).
;
(define (make-tree-get-min tree-ops)
 (define get (tree-op-get tree-ops))
 (define iter (tree-op-iter tree-ops))
 (define (break-on-first item) item)

 (lambda (tree-node)
  (iter tree-node break-on-first)
 )
)
