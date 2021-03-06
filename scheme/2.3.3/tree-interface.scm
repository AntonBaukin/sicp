
;
; The following tree op accessors define binary
; tree interface. A tree is defined by it's root
; node. Mutators of a tree always return the root
; reference that may (but not always) be altered.
;
; Each implementation of a tree returns an ops list
; with functions placed by well-known indices —
; this is the interface list.
;
; Shortcut operation means that it's not implemented
; as the tree level function, but are derived from.
; (It's not in the intreface list.)
;
; Heavyweight utility functions that may be implemented
; as shortcuts are still done on the tree level.
;
; This tree is not supposed to be a bag: it may not
; contain cmd-equal items (not strictly smaller).
;


; Comparison operator used to create this tree ops «class».
; It's asked mostly by the utility funtions. This accessor
; allows not carry this comparator alongside the tree ops.
(define (tree-op-smaller? tree-ops)
 (list-ref tree-ops 0)
)

; Returns sorted Set ops «class» with the same comparator.
; Also used by the utilities that work with sorted sets.
(define (tree-op-Set tree-ops)
 (list-ref tree-ops 1)
)

; Returns the value of the give node.
; Arguments: (tree-node).
(define (tree-op-get tree-ops)
 (list-ref tree-ops 2)
)

; Returns the left branch (sub-tree) of the give node.
; Null list '() means absence of the sub-tree.
; Arguments: (tree-node).
(define (tree-op-left tree-ops)
 (list-ref tree-ops 3)
)

(define (tree-op-right tree-ops)
 (list-ref tree-ops 4)
)

; Creates root node without the children. Note that you
; may not use null-list '() to denote empty tree.
; Arguments: (item).
(define (tree-op-make-node tree-ops)
 (list-ref tree-ops 5)
)

; Clones the tree structure using the same items.
; Arguments: (tree-node).
(define (tree-op-clone tree-ops)
 (list-ref tree-ops 6)
)

; Shortcut that tells whether the given node is a leaf.
; Arguments: (tree-node).
(define (tree-op-leaf? tree-ops)
 (define get-left (tree-op-left tree-ops))
 (define get-right (tree-op-right tree-ops))

 (lambda (node)
  (and (null? (get-left node)) (null? (get-right node)))
 )
)

; Searches for the given item and returns the actual
; item stored in the tree. Returns '() on not found.
;
; Returning stored item (instead of a boolean) allows
; to get whole data when the tree items comparator
; uses only the keys. This makes a tree to be like
; in-memory database (index).
;
; Arguments: (tree-node, phony item).
;
(define (tree-op-search tree-ops)
 (list-ref tree-ops 7)
)

; Creates sorted list from the given tree.
; Arguments: (tree-node).
(define (tree-op->list tree-ops)
 (list-ref tree-ops 8)
)

; Creates tree from the given list. This list may be any:
; not sorted, or contain duplicates (that ares removed).
; Arguments: (any list of items).
(define (tree-op<-list tree-ops)
 (list-ref tree-ops 9)
)

; Iterates over the tree in the cmp-order ascending passing
; items to the callback. Callback may stop (find behaviour).
;
; Arguments: (tree-node, callback).
; Callback arguments: (stored-item).
;
; As callback takes stored item, it may not alter the fields
; used for the comparison (not to break the tree order).
;
; Callback must return void to continue the iteration; on else
; values the iteration is breaked, and this value becomes
; the iteration result (you may also return the item).
;
(define (tree-op-iter tree-ops)
 (list-ref tree-ops 10)
)

; Inverses the control over «tree-op-iter».
;
; It returns an iteration function that provides tree nodes.
; Function returns empty list when it reaches the tree end.
;
; Arguments: (tree-node).
(define (tree-op-iterator table-ops)
 (list-ref table-ops 11)
)

; Adds item to the tree and returns new root node.
; If item was in the tree, replaces it. Balanced
; trees keep their balance.
;
; Note that the root may be '() to denote absent tree.
; In this case a root node is created.
;
; Arguments: (tree-node, item).
(define (tree-op-add tree-ops)
 (list-ref tree-ops 12)
)

; Removes item from the tree and returns new root node.
; Balanced trees keep their balance.
;
; Arguments: (tree-node, item).
(define (tree-op-delete tree-ops)
 (list-ref tree-ops 13)
)

; Returns length of the tree (longest root-node way).
; Arguments: (tree-node).
(define (tree-op-length tree-ops)
 (define get-left (tree-op-left tree-ops))
 (define get-right (tree-op-right tree-ops))

 (define (tree-length node)
  (if (null? node) 0
   (+ 1
    (max
     (tree-length (get-left node))
     (tree-length (get-right node))
    )
   )
  )
 )

 tree-length
)

; Returns size of the tree (number of nodes).
; Arguments: (tree-node).
(define (tree-op-size tree-ops)
 (define get-left (tree-op-left tree-ops))
 (define get-right (tree-op-right tree-ops))

 (define (tree-size node)
  (if (null? node) 0
   (+ 1
    (tree-size (get-left node))
    (tree-size (get-right node))
   )
  )
 )

 tree-size
)
