(include "sorted-set.scm")

; Creates collection of utilities related to a tree
; packed into a single ops-list, i.e. a «class».
; Use tree-op-* selectors of the ops.
; Comparator is as < operator.
(define (make-tree smaller?)

 (define nil '())

 (define Set (make-sorted-set smaller?))
 (define make-set (set-op-make Set))

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

 (define (search-node node item)
  (cond
   ((null? node) '())

   ((smaller? item (get node))
    (search-node (get-left node) item)
   )

   ((smaller? (get node) item)
    (search-node (get-right node) item)
   )

   (else node) ;<— found it
  )
 )

 (define (search tree item)
  (let ((node (search-node tree item)))
   (if (null? node) '() (get node))
  )
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

 (define (tree->list t)
  (if (null? t) t
   (append
    (tree->list (get-left t))
    (cons (get t) (tree->list (get-right t)))
    )
  )
 )

 ; Goes to the first item dividing the list by 2 on each step
 ; combining the left and rights sub-trees with the center
 ; item to be the node (local root) up to the very center
 ; of the initial list making it the resulting root.
 ; Takes o(n) steps.
 (define (partial-tree elements n)
  (if (= 0 n) (cons '() elements)
   (let* (
     (left-size (quotient (- n 1) 2))
     (left-result (partial-tree elements left-size))
     (left-tree (car left-result))
     (non-left-elements (cdr left-result))
     (right-size (- n (+ left-size 1)))   ; +1 as we take leading item
     (this-entry (car non-left-elements)) ; as this-entry of the node
     (right-result (partial-tree (cdr non-left-elements) right-size))
     (right-tree (car right-result))
     (remaining-elements (cdr right-result))
    )

    (cons
     (node this-entry left-tree right-tree)
     remaining-elements
    )
   )
  )
 )

 (define (list->tree sequence)
  (let ((seq (make-set sequence)))
   (car (partial-tree seq (length seq)))
  )
 )

 ;      0     1        2        3    4     5
 (list get get-left get-right node single leaf?
 ;        6        7       8     9     10
       set-left set-right set search smaller?
 ;         11         12     13
       tree->list list->tree Set )
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

; Searches for the given item and returns the item
; stored in the tree — this allows to search actual
; records by phony ones having only the keys set.
; Returns '() on not found.
(define (tree-op-search treeops)
 (list-ref treeops 9)
)

; Comparison operator used to create this tree class.
(define (tree-op-smaller? treeops)
 (list-ref treeops 10)
)

; Creates sorted list from the given tree.
(define (tree-op->list treeops)
 (list-ref treeops 11)
)

; Creates tree from the given list that may be not sorted
; and may contain duplicates (that ares removed).
(define (tree-op<-list treeops)
 (list-ref treeops 12)
)

; Returns sorted Set ops «class» with the same comparator.
(define (tree-op-Set treeops)
 (list-ref treeops 13)
)
