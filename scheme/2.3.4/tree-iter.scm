
; Creates iteration function for the given tree.
; The function takes (tree iter), where «iter» is
; function taking (node from stack) and returning:
; 'left — to go left, 'right — to go right, 'up —
; to return to the parent of the current node; and
; else value as the result. «Stack» is the stack of
; up-nodes of the current path. «From» is previously
; invoked node: initially is '(), on going down it's
; the top of the stack, on returning up — the left
; or the right node. If going to the requested direction
; is not possible, iteration is breaked returning pair
; (last node . stack)
(define (make-tree-iter treeops)
 (define left (tree-op-get-left treeops))
 (define right (tree-op-get-right treeops))

 (lambda (tree iter)
  (define (navigate node from stack)
   (let ((dir (iter node from stack)))
    (cond

     ((eq? 'left dir)
      (if (null? (left node)) node
       (navigate (left node) node (cons node stack))
      )
     )

     ((eq? 'right dir)
      (if (null? (right node)) node
       (navigate (right node) node (cons node stack))
      )
     )

     ((eq? 'up dir)
      (if (null? stack) node
       (navigate (car stack) node (cdr stack))
      )
     )

     (else (cons node stack))
    )
   )
  )

  (navigate tree '() '())
 )
)

; Creates iteration function that walks the tree from the
; minimum item to the maximum. The function takes (tree iter),
; where «iter» is function taking the value (item) of a node.
; When iterator returns '() it asks to continue, else value
; is treated as overall iteration result, it breaks iteration.
(define (make-tree-order-iter treeops)
 (define get (tree-op-get treeops))
 (define left (tree-op-get-left treeops))
 (define right (tree-op-get-right treeops))
 (define tree-iter (make-tree-iter treeops))

 (define (left? node)
  (not (null? (left node)))
 )

 (define (right? node)
  (not (null? (right node)))
 )

 (define (from-up? from stack)
  (eq? from (if (null? stack) '() (car stack)))
 )

 (define (from-right? node from)
  (eq? from (right node))
 )

 (define (call-or-go iter node dir)
  (let ((res (iter (get node))))
   ;(log "go " (if (null? res) dir res))
   (if (null? res) dir res)
  )
 )

; (define (log-stack stack)
;  (define (iter i tail)
;   (cond ((null? tail) void) (else
;    (display i) (display ": ")
;    (display (get (car tail)))
;    (newline)
;    (iter (+ i 1) (cdr tail))
;   ))
;  )
;  (if (null? stack) "EMPTY" (iter 0 stack))
; )

 (lambda (tree iter)
  (tree-iter tree (lambda (node from stack)

   ;(log "> " (get node))
   ;(log "< " (if (null? from) "ROOT" (get from)))
   ;(log-stack stack)

   (cond

    ;{ we came from the parent and may go left }
    ((and (left? node) (from-up? from stack))
     'left
    )

    ;{ we came not from the right and may go there}
    ((and (right? node) (not (from-right? node from)))
     (call-or-go iter node 'right)
    )

    ;{ have no right }
    ((not (right? node))
     (call-or-go iter node 'up)
     'up
    )

    ;~> came from the right
    (else 'up)
   )
  ))
 )
)

; For the given tree find the minimum (left-most) item.
(define (make-tree-get-min treeops)
 (define get (tree-op-get treeops))
 (define left (tree-op-get-left treeops))
 (define tree-iter (make-tree-iter treeops))

 (define (left? node)
  (not (null? (left node)))
 )

 (lambda (tree)
  (get (car (tree-iter tree (lambda (node from stack)
   (if (left? node) 'left node)
  ))))
 )
)

; For the given tree find the maximum (right-most) item.
(define (make-tree-get-max treeops)
 (define get (tree-op-get treeops))
 (define right (tree-op-get-right treeops))
 (define tree-iter (make-tree-iter treeops))

 (define (right? node)
  (not (null? (right node)))
 )

 (lambda (tree)
  (get (car (tree-iter tree (lambda (node from stack)
   (if (right? node) 'right node)
  ))))
 )
)

; For the given (tree item) find the largest
; item that is smaller than the given one, i.e.,
; the previous. The given item may absent.
(define (make-tree-find-smaller treeops)
)