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


 (define Set (make-sorted-set smaller?))
 (define make-set (set-op-make Set))

 (define (make-root item)
  (cons item (cons '() '()))
 )

 (define (get node)
  (car node)
 )

 (define (get-left node)
  (cadr node)
 )

 (define (get-right node)
  (cddr node)
 )


 ; Resulting operations set:
 (compose-list
  (list
   smaller?     ; 0
   Set          ; 1
   get          ; 2
   get-left     ; 3
   get-right    ; 4
   make-root    ; 5
  )

  ; search @ 6
  make-tree-op-search

  ; tree->list @ 7, list->tree @ 8
  (curry make-tree-op-list make-tree-node)

  ; iter @ 9
  make-tree-op-iter
 )
)
