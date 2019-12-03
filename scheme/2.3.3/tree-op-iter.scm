
; Creates iteration function for the given tree ops.
; Traverses the tree in the sort ascending order.
; Iterator arguments: (root-node callback).
;
; Callback takes the item and must return void to
; continue the iteration. Other value is treated as
; break command, it's returned as the iteration
; result (find behaviour).
;
(define (make-tree-op-iter tree-ops)
 (define get (tree-op-get tree-ops))
 (define get-left (tree-op-left tree-ops))
 (define get-right (tree-op-right tree-ops))


 (define (iter-right node cb)
  (let ((res (cb (get node))))
   ; Invoke callback for the current node:
   (if (eq? void res)
    ; Recurse into the right node:
    (if (null? (get-right node))
     void ;<— finished the iteration for this node
     (iter (get-right node) cb)
    )
    res ;<— return this result, break the iteration
   )
  )
 )

 (define (iter node cb)
  (cond
   ((null? node) void)

   ((null? (get-left node))
    (iter-right node cb)
   )

   (else ; First, go left, then this and right:
    (let ((res (iter (get-left node) cb)))
     (if (eq? void res)
      (iter-right node cb)
      res ;<— the left had breaked
     )
    )
   )
  )
 )


 (list iter) ;<— compose list ready
)
