
; Creates iteration function for the given tree.
;
; Arguments: (tree-node iterator).
; Iterator arguments: (node from stack).
;
; «Stack» is the stack of up-nodes of the current path
; (excluding node). For a root node stack is '().
;
; «From» is previously invoked node: initially is '(),
; on going down it's the top of the stack, on returning
; up — the leftor the right child node.
;
; Iterator results: 'left — to go left, 'right — to go
; right, 'up — to return to the parent of the current
; node; 'root — to start clear from the root, and else
; value as the result that breaks the iteration: list
; ('result result node from stack).
;
; Hint: iteration is also breaked when going to the
; root from this root — this prevents loops.
;
; If going to the returned way is not possible, iteration
; is breaked returning: list ('break way node from stack).
;
(define (make-tree-util-walker tree-ops)
 (define get-left (tree-op-left tree-ops))
 (define get-right (tree-op-right tree-ops))

 (define (break way node from stack)
  (list 'break way node from stack)
 )

 (define (navigate iter node from stack)
  (let ((way (iter node from stack)))
   (cond

    ((eq? 'left way)
     (if (null? (get-left node)) (break way node from stack)
      (navigate iter (get-left node) node (cons node stack))
     )
    )

    ((eq? 'right way)
     (if (null? (get-right node)) (break way node from stack)
      (navigate iter (get-right node) node (cons node stack))
     )
    )

    ((eq? 'up way)
     (if (null? stack) (break way node from stack)
      (navigate iter (car stack) node (cdr stack))
     )
    )

    ((eq? 'root way)
     (if (null? stack) (break way node from stack)
      (navigate iter (car (reverse stack)) '() '())
     )
    )

    (else (list 'result way node from stack))
   )
  )
 )

 (lambda (tree iter)
  (navigate iter tree '() '())
 )
)
