; Depends on «../3.3.3/tree-red-black.scm».


; Index tree «class». Items of this tree are pairs having
; the first item a number key, and second is the value.
(define IndexTree (make-rb-tree (lambda (a b) (< (car a) (car b)))))

(define (index-tree-get itree) (list-ref itree 0))
(define (index-tree-set itree) (list-ref itree 1))
(define (index-tree-del itree) (list-ref itree 2))
(define (index-tree-root itree) (list-ref itree 3))
(define (index-tree-iter itree) (list-ref itree 4))

; Creates index tree instance and returns ops set for this
; particular instnace. Index tree has numbers as the keys.
; The three is binary sorted. Yoy may access the root node
; and use operations from «tree.scm» and «tree-iter.scm».
;
; Resulting operations list is:
;  0 — get value by number key;
;  1 — set value by number key;
;  2 — delete value by number key;
;  3 — getter of the tree root;
;  4 - iterator by index-order.
;
; Get returns '() when value is not found.
;
; Iterator callback takes (index, item) args
; and returns void to continue iteration; else
; result breaks it and is returned out.
;
(define (make-index-tree)
 (define tree-add (tree-op-add IndexTree))
 (define tree-del (tree-op-delete IndexTree))
 (define tree-iter (tree-op-iter IndexTree))

 (define instance '()) ;<— tree instance

 (define (get-kv key)
  (if (number? key)
   ((tree-op-search IndexTree) instance (cons key '()))
   (error "Not a number key for index tree!" key)
  )
 )

 (define (get key)
  (let ((kv (get-kv key)))
   (if (null? kv) '() (cdr kv))
  )
 )

 (define (set key value)
  (let ((kv (get-kv key)))
   (if (null? kv) ; Not found the key-value pair?
    (set! instance (tree-add instance (cons key value)))
    (set-cdr! kv value) ;<— update it in-place
   )
  )
 )

 (define (del key)
  (set! instance (tree-del instance (cons key '())))
 )

 (define (tree) instance)

 (define (iter cb)
  ((tree-iter instance
   (lambda (kv) (cb (car kv) (cdr kv)))
  ))
 )

 ; Resulting instance bound operations:
 (list get set del tree iter)
)
