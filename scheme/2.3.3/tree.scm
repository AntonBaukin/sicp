(include "sorted-set.scm")
(include "tree-interface.scm")


; This low-lever function allows to create arbitrary binary
; trees implemented in this package regardless of the order
; specified by the comparator.
;
; This may be allowed for test purposes where the order
; of the items has no meaning. We do not include genaral
; maker in abstract tree interface as only the order and
; tree balance features define what's on the left-right.
;
(define (make-tree-node item left right)
 (cons item (cons left right))
)

; Creates collection of ops for general binary tree.
; Use tree-op-* interface to access these operations.
; Comparator smaller? is a binary predicate.
(define (make-tree smaller?)
 (include "curry.scm")
 (include "compose-list.scm")
 (include "tree-op-search.scm")
 (include "tree-op-list.scm")
 (include "tree-op-iter.scm")
 (include "tree-op-add.scm")
 (include "tree-op-delete.scm")


 (define Set (make-sorted-set smaller?))
 (define make-set (set-op-make Set))

 (define (make-node item)
  (cons item (cons '() '()))
 )

 (define (get node)
  (car node)
 )

 (define (set node item)
  (set-car! node item)
 )

 (define (get-left node)
  (cadr node)
 )

 (define (set-left node left)
  (set-car! (cdr node) left)
 )

 (define (get-right node)
  (cddr node)
 )

 (define (set-right node right)
  (set-cdr! (cdr node) right)
 )

 (define (trace-root node stack)
  (if (null? stack) node
   (trace-root (car stack) (cdr stack))
  )
 )

 (define (add-update cmd item node stack)
  (cond
   ((eq? 'L cmd) (set-left node (make-node item)))
   ((eq? 'R cmd) (set-right node (make-node item)))
   (else (set node item))
  )

  ; Always return the root node:
  (trace-root node stack)
 )

 (define (replace-child stack child with)
  (cond
   ((null? stack) with)

   ((eq? child (get-left (car stack)))
    (set-left (car stack) with)
    (trace-root '() stack)
   )

   (else
    (set-right (car stack) with)
    (trace-root '() stack)
   )
  )
 )

 (define (delete status node stack next-node next-stack)
  (cond
   ((eq? '0 status)
    (replace-child stack node '())
   )

   ((eq? 'L status)
    (replace-child stack node (get-left node))
   )

   ((eq? 'R status)
    (replace-child stack node (get-right node))
   )

   (else
    ; As the next node has no left child, we first
    ; assign it as the left child of the removed one:
    (set-left next-node (get-left node))

    ; If next node has parent being the deleted node,
    ; we just replace it. Else, we first detach the
    ; next node from it's parent, then swap them
    (if (null? next-stack)
     (replace-child stack node next-node)
     (begin
      (replace-child next-stack next-node (get-right next-node))
      (set-right next-node (get-right node))
      (replace-child stack node next-node)
     )
    )
   )
  )
 )

 (define (clone-node node)
  (if (null? node) '()
   (cons
    (get node)
    (cons
     (clone-node (get-left node))
     (clone-node (get-right node))
    )
   )
  )
 )

 (define (make-tree-node-op-list item left right build)
  (make-tree-node item left right)
 )

 (define (post-build-op-list build root-node)
  root-node
 )
 

 ; Resulting operations set:
 (compose-list
  (list
   smaller?     ; 0
   Set          ; 1
   get          ; 2
   get-left     ; 3
   get-right    ; 4
   make-node    ; 5
   clone-node   ; 6
  )

  ; search @ 7
  make-tree-op-search

  ; tree->list @ 8, list->tree @ 9
  (curry
   make-tree-op-list
   make-tree-node-op-list
   post-build-op-list
  )

  ; iter @ 10
  make-tree-op-iter

  ; add @ 11
  (curry make-tree-op-add add-update)

  ; delete @ 12
  (curry make-tree-op-delete delete)
 )
)
