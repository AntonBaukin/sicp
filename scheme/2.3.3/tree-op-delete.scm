
; Creates binary tree delete navigator. It traverses the tree
; and finds node with the target item. Then it invokes delete
; callback to detacth the node.
;
; Operation arguments: (root-node, item).
;
; Delete arguments: (status node stack next-node next-stack),
; where status tells the status of the node:
;  '0 — node is a leaf, just detach it,
;  'L — node has single left child;
;  'R — node has single right child;
;  '2 - node has two children (see below).
;
; Zero item in the stack is the parent of the target node;
; other items are the way-up to the root (being the last).
;
; Arguments (next-node next-stack) are not '() only in '2.
; In this case the next node has no left child, it's in the
; right sub-tree of the node to delete. Next stack is the up
; way back to the node to delete, not including it.
;
; Delete callback must return a new root node of the tree.
; When item is not found in the tree, nothing is updated,
; and the same root is always returned.
;
(define (make-tree-op-delete delete tree-ops)
 (define smaller? (tree-op-smaller? tree-ops))
 (define get (tree-op-get tree-ops))
 (define get-left (tree-op-left tree-ops))
 (define get-right (tree-op-right tree-ops))


 ; When item to delete is not found, we just return
 ; the last item of the stack — the root, or current
 ; node when the stack is empty (it's the root).
 (define (trace-root node stack)
  (if (null? stack) node
   (trace-root (car stack) (cdr stack))
  )
 )

 ; Finds the minimum node of the tree.
 ; Returns pair (node . stack).
 (define (trace-left node stack)
  (if (null? (get-left node))
   (cons node stack)
   (trace-left (get-left node) (cons node stack))
  )
 )

 (define (navigate node item stack)
  (cond
   ((smaller? item (get node))
    (if (null? (get-left node))
     (trace-root node stack)
     (navigate (get-left node) item (cons node stack))
    )
   )

   ((smaller? (get node) item)
    (if (null? (get-right node))
     (trace-root node stack)
     (navigate (get-right node) item (cons node stack))
    )
   )

   (else ; We found the target item, proceed with the cases:
    (cond
     ((and (null? (get-left node)) (null? (get-right node)))
      (delete '0 node stack '() '())
     )

     ((null? (get-left node))
      (delete 'R node stack '() '())
     )

     ((null? (get-right node))
      (delete 'L node stack '() '())
     )

     (else
      (let ((tr (trace-left (get-right node) '())))
       (delete '2 node stack (car tr) (cdr tr))
      )
     )
    )
   )
  )
 )

 (define (delete-node root-node item)
  (navigate root-node item '())
 )


 (list delete-node) ;<— compose list ready
)
