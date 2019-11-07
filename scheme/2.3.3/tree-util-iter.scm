; Depends on «curry.scm».
; Depends on «tree-util-walk.scm».

; Creates function that takes binary tree and
; returns in-order iterator. The iteration is
; based on the walker, iterator receives the
; same arguments, but it's result values is
; treated differently: void result asks to
; continue, else result breaks.
;
; Comparing with the iterator build into the
; tree ops, this is invoked with node (not it's
; value item), along with the up-stack. 
;
; Arguments: (tree-node iter).
; Optional utilities: (walker).
;
; Walker for the same tree-ops may be given
; for reuse purposes. It's created on demand.
;
; Iterator arguments: (node stack).
; See «tree-util-walk.scm».
;
(define (make-tree-util-iter tree-ops . utils)
 (define get-left (tree-op-left tree-ops))
 (define get-right (tree-op-right tree-ops))

 (define walker
  (if (= 1 (length utils))
   (list-ref utils 0)
   (make-tree-util-walker tree-ops)
  )
 )

 (define (has-left? node)
  (not (null? (get-left node)))
 )

 (define (has-right? node)
  (not (null? (get-right node)))
 )

 (define (from-up? from stack)
  (eq? from (if (null? stack) '() (car stack)))
 )

 (define (from-right? node from)
  (eq? from (get-right node))
 )

 (define (call-or-go dir iter node from stack)
  (let ((res (iter node stack)))
   (if (eq? void res) dir res)
  )
 )

 (define (navigate iter node from stack)
  (cond

   ;{ came from the parent and may go left }
   ((and (has-left? node) (from-up? from stack))
    'left
   )

   ;{ came not from the right and may go there}
   ((and (has-right? node) (not (from-right? node from)))
    (call-or-go 'right iter node from stack)
   )

   ;{ have no right }
   ((not (has-right? node))
    (call-or-go 'up iter node from stack)
    'up
   )

   (else 'up) ;<— came from the right
  )
 )

 (lambda (tree-node iter)
  (walker tree-node (curry navigate iter))
 )
)
