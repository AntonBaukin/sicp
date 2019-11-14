(include "../2.3.3/sorted-set.scm")
(include "../2.3.3/tree-interface.scm")

; This package implements red-black balanced binary tree.
; It produces the same interface ops as «2.3.3/tree.scm».
;
; Each red-black tree has this invariant true:
;
; 1) root node is black;
; 2) red node has black children;
; 3) each root-leaf path has the same number of black nodes.
;
; A new node is first inserted as a leaf in proper order
; position (as it's for every ordered binary tree). It's
; assigned red color. Then in the case of rule 2) violation,
; the sub-tree is rotated on the way up.
;


(define (make-rb-tree smaller?)
 (include "../2.3.3/curry.scm")
 (include "../2.3.3/compose-list.scm")
 (include "../2.3.3/tree-op-search.scm")
 (include "../2.3.3/tree-op-list.scm")
 (include "../2.3.3/tree-op-iter.scm")
; (include "../2.3.3/tree-op-add.scm")
; (include "../2.3.3/tree-op-delete.scm")


 (define Set (make-sorted-set smaller?))
 (define make-set (set-op-make Set))

 (define (make-node item)
  (list item 'black '() '())
 )

 (define (get node)
  (car node)
 )

 ; This operation may be invoked on a node when it
 ; doesn't alter the tree order. It's an internal op.
 ; The same it true for each set-op below.
 (define (set node item)
  (set-car! node item)
 )

 (define (get-color node)
  (cadr node)
 )

 (define (red? node)
  (eq? 'red (get-color node))
 )

 (define (set-red node)
  (set-car! (cdr node) 'red)
  node
 )

 (define (black? node)
  (eq? 'black (get-color node))
 )

 (define (set-black node)
  (set-car! (cdr node) 'black)
  node
 )

 (define (get-left node)
  (caddr node)
 )

 (define (set-left node left)
  (set-car! (cddr node) left)
 )

 (define (get-right node)
  (cadddr node)
 )

 (define (set-right node right)
  (set-car! (cdddr node) right)
 )

 (define (clone-node node)
  (if (null? node) '()
   (list
    (get node)
    (get-color node)
    (clone-node (get-left node))
    (clone-node (get-right node))
   )
  )
 )

 (define (make-tree-node-op-list item left right)
  (list item 'black left
   (if (and (not (null? right)) (null? left))
    (set-red right)
    right
   )
  )
 )

; (define (trace-root node stack)
;  (if (null? stack) node
;   (trace-root (car stack) (cdr stack))
;  )
; )

 

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
  (curry make-tree-op-list make-tree-node-op-list)

  ; iter @ 10
  make-tree-op-iter

  ; add @ 11
  ;(curry make-tree-op-add add-update)
  (lambda (l) (list '()))

  ; delete @ 12
  ;(curry make-tree-op-delete delete)
  (lambda (l) (list '()))
 )
)
