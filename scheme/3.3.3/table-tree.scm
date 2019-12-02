; Depends on «../3.3.3/tree-red-black.scm», if
; created with <? predicate instead of tree ops.

(include "table-base.scm")

; Table map is a bridge between a tree and table:
; the constructor given takes key<? predicate.
;
(define (make-table-tree key<?)
 ; Inner tree stores (key . value) pair as a value.
 (define TreeOps (make-rb-tree
  (lambda (a b) (key<? (car a) (car b)))
 ))

 (define tree-search (tree-op-search TreeOps))
 (define tree-add (tree-op-add TreeOps))
 (define tree-iter (tree-op-iter TreeOps))
 (define tree-size (tree-op-size TreeOps))

 (define (make) '())

 (define (search tree key)
  (let ((kv (tree-search tree (cons key '()))))
   (if (null? kv) void kv)
  )
 )

 (define (save tree key value)
  (tree-add tree (cons key value))
 )

 (define (rewrite tree kv value)
  (set-cdr! kv value)
  tree
 )

 (define (iter tree visitor)
  (tree-iter tree
   (lambda (kv)
    (let ((res (visitor (car kv) (cdr kv))))
     (cond
      ((eq? #f res) #f) ;<— do break
      ((eq? void res) void)
      (else (set-cdr! kv res) void)
     )
    )
   )
  )
 )

 (make-table-base make search save rewrite tree-size iter)
)
