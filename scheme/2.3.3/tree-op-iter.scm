
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

; Creates factory of tree iterators defined
; by «tree-op-iterator» interface.
(define (make-tree-op-iterator tree-ops)
 (define get-left (tree-op-left tree-ops))
 (define get-right (tree-op-right tree-ops))

 ; Stack items are pairs (state . node), where state index is:
 ;  0 — initial, go left;
 ;  1 - up from left, returned this node;
 ;  2 - go right;
 ;  3 — done, go up.
 (define (push stack node)
  (cons (cons 0 node) stack)
 )

 ; Returns node of the stack top.
 (define (get stack) (cdar stack))

 ; Tests the state of the stack top.
 (define (left? stack) (eq? 0 (caar stack)))
 (define (this? stack) (eq? 1 (caar stack)))
 (define (right? stack) (eq? 2 (caar stack)))

 ; Increments the top's state.
 (define (inc stack)
  (set-car! (car stack) (+ 1 (caar stack)))
  stack
 )

 ; Goes to the next item of the tree and returns the new stack.
 ; The node on the top of the stack is the next to return.
 (define (next stack)
  (cond
   ((left? stack) (go-left stack))
   ((this? stack) (inc stack))
   ((right? stack) (go-right stack))
   (else (go-up stack))
  )
 )

 (define (go-left stack)
  ; (log-stack stack 'GoL)
  (if (null? (get-left (get stack)))
   (next (inc stack))
   (go-left (push (inc stack) (get-left (get stack))))
  )
 )

 (define (go-right stack)
  ; (log-stack stack 'GoR)
  (if (null? (get-right (get stack)))
   (next (inc stack))
   (go-left (push (inc stack) (get-right (get stack))))
  )
 )

 (define (go-up stack)
  ; (log-stack stack 'GoU)
  (if (null? (cdr stack))
   '() ;<— done traversing
   (next (cdr stack))
  )
 )

 ; (define op-get (tree-op-get tree-ops))
 ;
 ; (define (log-get stack)
 ;  (if (null? stack) 'EMPTY (op-get (get stack)))
 ; )
 ;
 ; (define (log-stack stack op)
 ;  (log op " " (map (lambda (x) (cons (car x) (op-get (cdr x)))) stack))
 ; )

 (define (iter-factory node)
  (define stack (push '() node))

  (lambda ()
   (if (null? stack) '()
    (begin
     (set! stack (next stack))
     ; (log-stack stack '>>)
     (if (null? stack) '() (get stack))
    )
   )
  )
 )

 (list iter-factory) ;<— compose list ready
)
