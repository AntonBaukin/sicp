
; Index tree «class». Items of this tree are pairs having
; the first item a number key, and second is the value.
(define IndexTree (make-tree (lambda (a b) (< (car a) (car b)))))

(define (index-tree-get itree) (list-ref itree 0))
(define (index-tree-set itree) (list-ref itree 1))
(define (index-tree-root itree) (list-ref itree 2))
(define (index-tree-iter itree) (list-ref itree 3))

; Creates index tree instance and returns ops set for this
; particular instnace. Index tree has numbers as the keys.
; The three is binary sorted. Yoy may access the root node
; and use operations from «tree.scm» and «tree-iter.scm».
;
; Resulting operations list is:
;  0 — get value by number key;
;  1 — set value by number key;
;  2 — getter of the tree root;
;  3 - iterator by index-order.
;
; Get returns '() when value is not found.
;
; Iterator callback takes (index, item) args
; and returns void to continue iteration; else
; result breaks it and is returned out.
;
(define (make-index-tree)
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
   ; Found the key-value pair? Update it in-place:
   (if (not (null? kv))
    (set-cdr! kv value)

    ; To add item we first convert the tree to the list,
    ; then add the new key-value pair, then convert back.
    ; It's bad, but else we have to create true b-tree.
    (set! instance ((tree-op<-list IndexTree)
     ((set-op-add (tree-op-Set IndexTree))
      ((tree-op->list IndexTree) instance)
      (cons key value)
     ))
    )
   )
  )
 )

 (define (tree) instance)

 (define (iter cb)
  (((tree-op-iter IndexTree) instance
   (lambda (kv) (cb (car kv) (cdr kv)))
  ))
 )

 ; Resulting instance bound operations:
 (list get set tree iter)
)
