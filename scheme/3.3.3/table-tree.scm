; Depends on «../3.3.3/tree-red-black.scm».
(include "table-base.scm")

; Table map is a bridge between a tree and table:
; the constructor given takes key<? predicate.
;
(define (make-table-tree key<?)
 ; Inner tree stores (key . value) pair as a value.
 (define TreeOps (make-rb-tree
  (lambda (a b) (key<? (car a) (car b)))
 ))

 (define tree-get (tree-op-get TreeOps))
 (define tree-search (tree-op-search TreeOps))
 (define tree-add (tree-op-add TreeOps))
 (define tree-delete (tree-op-delete TreeOps))
 (define tree-iterator (tree-op-iterator TreeOps))
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

 (define (without tree key)
  (tree-delete tree (cons key '()))
 )

 ; Here kvx is a list of (k v node).
 (define (iter-set kvx value)
  (define node (caddr kvx))
  (define kv (tree-get node))
  (set-cdr! kv value)
 )

 (define (iter tree)
  (define it (tree-iterator tree))
  ; We cache «kvn» instance as it's allowed by the interface:
  (define x (cons '() '()))
  (define vx (cons '() x))
  (define kvx (cons '() vx))
  (define kvn (cons kvx iter-set))

  (lambda ()
   (define node (it))

   (if (null? node) '()
    (let ((kv (tree-get node)))
     (set-car! x node) ;<— save the node
     (set-car! vx (cdr kv))
     (set-car! kvx (car kv))
     kvn ;<— always return the same instance
    )
   )
  )
 )

 (make-table-base make search save rewrite without tree-size iter)
)
