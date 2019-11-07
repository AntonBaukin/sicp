; Depends on «tree-util-walk.scm».

; Creates function that takes binary tree and returns
; list with lists of (node parent .. root) that allows
; to navigate back to the top.
;
; Arguments: (tree-node).
;
(define (make-tree-get-leafs tree-ops)
 (define iter (tree-op-iter tree-ops))

 (lambda (tree-node)
  (define result '())

  (iter tree-node
   (lambda (node from stack)
    (cond
     ((leaf? node)
      (set! result (cons (cons node stack) result))
      void ;<— request the next
     )
     (else void)
    )
   )
  )

  (reverse result)
 )
)
