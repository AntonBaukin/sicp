
; Creates collection of utilities related to a tree
; packed into a single ops-list, i.e. a «class».
; Use tree-op-* selectors of the ops.
; Comparator is as < operator.
(define (make-tree smaller?)

 (define nil '())

 (define (node item left right)
  (cons item (cons left right))
 )

 (define (single item)
  (cons item (cons nil nil))
 )

 (define (get node)
  (car node)
 )

 (define (set node item)
  (cons item (cdr node))
 )

 (define (get-left node)
  (cadr node)
 )

 (define (get-right node)
  (cddr node)
 )

 (define (set-left node branch)
  (cons (get node) (cons branch (get-right node)))
 )

 (define (set-right node branch)
  (cons (get node) (cons (get-left node branch)))
 )

 (define (leaf? node)
  (and (null? (get-left node)) (null? (get-right node)))
 )

 ;      0     1        2        3    4     5
 (list get get-left get-right node single leaf?
 ;        6        7       8     9
       set-left set-right set smaller?)
)

; Returns the value of the give node.
(define (tree-op-get treeops)
 (list-ref treeops 0)
)

; Returns the left branch (sub-tree) of the give node.
; Empty list '() means absence of the sub-tree.
(define (tree-op-get-left treeops)
 (list-ref treeops 1)
)

(define (tree-op-get-right treeops)
 (list-ref treeops 2)
)

; Creates tree node from three arguments: item, left and
; right sub-trees (branches, i.e. nodes) that are '() if empty.
(define (tree-op-node treeops)
 (list-ref treeops 3)
)

; Shortcut for node with item and empty branches.
(define (tree-op-single treeops)
 (list-ref treeops 4)
)

; Tells whether the given node is a leaf.
(define (tree-op-leaf? treeops)
 (list-ref treeops 5)
)

; Takes node and returns it's copy assigning new left
; branch that may be set '() to clear it.
(define (tree-op-set-left treeops)
 (list-ref treeops 6)
)

(define (tree-op-set-right treeops)
 (list-ref treeops 7)
)

; Takes node and returns it's copy assigning new item
; leaving the left and right branches intact.
(define (tree-op-set treeops)
 (list-ref treeops 8)
)

; Comparison operator used to create this tree class.
(define (tree-op-smaller? treeops)
 (list-ref treeops 9)
)
