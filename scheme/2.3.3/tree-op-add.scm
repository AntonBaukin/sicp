
; Creates binary tree add navigator. It traverses the tree and
; finds proper position to insert the given item as a leaf node.
;
; Operation arguments: (root-node, item).
;
; Update arguments: (cmd item node stack),
; where cmd tells what to do with the node:
;  'L — set new leaf as the left,
;  'R — set the right one, or
;  'U — update item in this node.
;
; 'U is when the same item is found in the tree, it tells
; to just update the existing node.
;
; Zero item in the stack is the parent of the target node;
; other items are the way-up to the root (being the last).
;
; Update callback must return a new root node of the tree.
;
(define (make-tree-op-add update tree-ops)
 (define smaller? (tree-op-smaller? tree-ops))
 (define get (tree-op-get tree-ops))
 (define get-left (tree-op-left tree-ops))
 (define get-right (tree-op-right tree-ops))


 (define (navigate node item stack)
  (cond
   ((smaller? item (get node))
    (if (null? (get-left node))
     (update 'L item node stack)
     (navigate (get-left node) item (cons node stack))
    )
   )

   ((smaller? (get node) item)
    (if (null? (get-right node))
     (update 'R item node stack)
     (navigate (get-right node) item (cons node stack))
    )
   )

   (else (update 'U item node stack))
  )
 )

 (define (add root-node item)
  (navigate root-node item '())
 )


 (list add) ;<— compose list ready
)
