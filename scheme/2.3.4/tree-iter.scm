(include "../2.3.3/quick-sort.scm")

; Creates iteration function for the given tree.
; The function takes (tree iter), where «iter» is
; function taking (node from stack) and returning:
; 'left — to go left, 'right — to go right, 'up —
; to return to the parent of the current node; and
; else value as the result. «Stack» is the stack of
; up-nodes of the current path. «From» is previously
; invoked node: initially is '(), on going down it's
; the top of the stack, on returning up — the left
; or the right node. If going to the requested way
; is not possible, iteration is breaked returning
; list ('break way node from stack). On else result,
; list ('result result node from stack) is returned.
; Special way 'root orders to start from the root.
(define (make-tree-iter treeops)
 (define left (tree-op-get-left treeops))
 (define right (tree-op-get-right treeops))

 (define (break way node from stack)
  (list 'break way node from stack)
 )

 (lambda (tree iter)
  (define (navigate node from stack)
   (let ((way (iter node from stack)))
    (cond

     ((eq? 'left way)
      (if (null? (left node)) (break way node from stack)
       (navigate (left node) node (cons node stack))
      )
     )

     ((eq? 'right way)
      (if (null? (right node)) (break way node from stack)
       (navigate (right node) node (cons node stack))
      )
     )

     ((eq? 'up way)
      (if (null? stack) (break way node from stack)
       (navigate (car stack) node (cdr stack))
      )
     )

     ((eq? 'root way) (navigate tree '() '()))

     (else (list 'result way node from stack))
    )
   )
  )

  (navigate tree '() '())
 )
)

; Creates iteration function that walks the tree from the
; minimum item to the maximum. The function takes (tree iter),
; where «iter» is function taking the value (item) of a node.
; When iterator returns void it asks to continue, else value
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
   ;(log "go " (if (eq? void res) dir res))
   (if (eq? void res) dir res)
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
 (define tree-iter (make-tree-iter treeops))

 (define (result r)
  (get (caddr r))
 )

 (lambda (tree)
  (result (tree-iter tree (lambda (node from stack) 'left)))
 )
)

; For the given tree find the maximum (right-most) item.
(define (make-tree-get-max treeops)
 (define get (tree-op-get treeops))
 (define tree-iter (make-tree-iter treeops))

 (define (result r)
  (get (caddr r))
 )

 (lambda (tree)
  (result (tree-iter tree (lambda (node from stack) 'right)))
 )
)

; For the given (tree item) finds the largest
; item that is smaller than the given one, i.e.,
; the previous one. The given item may absent.
(define (make-tree-get-max-smaller treeops)
 (define get (tree-op-get treeops))
 (define left (tree-op-get-left treeops))
 (define smaller? (tree-op-smaller? treeops))
 (define tree-iter (make-tree-iter treeops))
 (define get-max (make-tree-get-max treeops))

 (define (smaller-node? a b)
  (smaller? (get a) (get b))
 )

 (define (get-max-safe tree)
  ;(log "max " (if (null? tree) '() (get-max tree)) " of " tree)
  (if (null? tree) '() (get-max tree))
 )

 (define (min-max item node stack)
  (define (next res tail)
   (cond
    ((null? tail) res)

    ((smaller? (get (car tail)) item)
     (next (get (car tail)) (cdr tail))
    )

    (else res)
   )
  )

  ;(log "min-max " item " node = " node " stack = " stack)
  (next '() (quick-sort smaller-node? (cons node stack)))
 )

 ; When going left we put to the stack an item that is greater,
 ; right — the item is smaller. So, when breaked, we have stack
 ; that contains smaller and greater items, and we do min-max.
 (define (result item r)
  ;(log "result " item " " r)

  (if (and (eq? 'result (car r)) (not (null? (cadr r))))
   (cadr r) (min-max item (list-ref r 2) (list-ref r 4))
  )
 )

 (lambda (tree item)
  (result item (tree-iter tree (lambda (node from stack)
   ;(log "item = " item " node = " node)

   (cond
    ((smaller? item (get node)) 'left)
    ((smaller? (get node) item) 'right)

    ; { node's item equals to the item}
    (else (get-max-safe (left node)))
   )
  )))
 )
)


; For the given (tree item) finds the smallest
; item that is greater than the given one, i.e.,
; the next one. The given item may absent.
(define (make-tree-get-min-greater treeops)
 (define get (tree-op-get treeops))
 (define right (tree-op-get-right treeops))
 (define smaller? (tree-op-smaller? treeops))
 (define tree-iter (make-tree-iter treeops))
 (define get-min (make-tree-get-min treeops))

 (define (greater-node? a b)
  (smaller? (get b) (get a))
 )

 (define (get-min-safe tree)
  ;(log "min " (if (null? tree) '() (get-min tree)) " of " tree)
  (if (null? tree) '() (get-min tree))
 )

 (define (max-min item node stack)
  (define (next res tail)
   (cond
    ((null? tail) res)

    ((smaller? item (get (car tail)))
     (next (get (car tail)) (cdr tail))
    )

    (else res)
   )
  )

  ;(log "nin-max " item " node = " node " stack = " stack)
  (next '() (quick-sort greater-node? (cons node stack)))
 )

 (define (result item r)
  ;(log "result " item " " r)

  (if (and (eq? 'result (car r)) (not (null? (cadr r))))
   (cadr r) (max-min item (list-ref r 2) (list-ref r 4))
  )
 )

 (lambda (tree item)
  (result item (tree-iter tree (lambda (node from stack)
   ;(log "item = " item " node = " node)

   (cond
    ((smaller? item (get node)) 'left)
    ((smaller? (get node) item) 'right)

    ; { node's item equals to the item}
    (else (get-min-safe (right node)))
   )
  )))
 )
)