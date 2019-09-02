
; Index tree «class». Items of this tree are pairs having
; the first item a number key, and second is the value.
(define IndexTree (make-tree (lambda (a b) (< (car a) (car b)))))

(define IndexTreeSet       (tree-op-Set IndexTree))
(define index-tree-search  (tree-op-search IndexTree))
(define index-tree->list   (tree-op->list IndexTree))
(define list->index-tree   (tree-op<-list IndexTree))
(define index-tree-set-add (set-op-add IndexTreeSet))

; Creates index tree instance and returns ops set for this
; particular instnace. Index tree has numbers as the keys.
; The three is binary sorted. Yoy may access the root node
; and use operations from «tree.scm» and «tree-iter.scm».
;
; Resulting operations list is:
;  0 — get value by number key;
;  1 — set value by number key;
;  2 — getter of the tree root.
;
; Get returns '() when value is not found.
;
(define (make-index-tree)
 (define instance '()) ;<— tree instance

 (define (get-kv key)
  (if (number? key)
   (index-tree-search instance (cons key '()))
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
    (set! instance (list->index-tree
     (index-tree-set-add
      (index-tree->list instance)
      (cons key value)
     ))
    )
   )
  )
 )

 ; Resulting instance bound operations:
 (list get set (lambda () instance))
)
