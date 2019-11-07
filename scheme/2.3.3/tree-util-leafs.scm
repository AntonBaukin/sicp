; Depends on «tree-util-iter.scm».

; Creates function that takes binary tree and returns
; list with lists of (node parent .. root) that allows
; to navigate back to the root.
;
; Arguments: (tree-node).
; Optional utilities: (util-iter).
;
; Utilities iterator (not tree interface one!) for
; the same tree-ops may be given for reuse purposes.
; It's created on demand.
;
(define (make-tree-get-leafs tree-ops . utils)
 (define leaf? (tree-op-leaf? tree-ops))

 (define iter
  (if (= 1 (length utils))
   (list-ref utils 0)
   (make-tree-util-iter tree-ops)
  )
 )

 (lambda (tree-node)
  (define result '())

  (iter tree-node
   (lambda (node stack)
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
