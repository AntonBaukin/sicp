(define (log . args) (for-each display args) (newline))

(include "2.3.3-tree.scm")
(include "2.3.3-tree-print.scm")

(define StringTree (make-tree string-ci<?))
(define make-str-single (tree-op-single StringTree))
(define make-str-node (tree-op-node StringTree))
(define str-tree->str (make-tree-str-printer StringTree (lambda (s) s)))

(define test-str-tree (make-str-node "a"
 (make-str-node "b" (make-str-single "d") (make-str-single "e"))
 (make-str-node "c" (make-str-single "f") (make-str-single "g"))
))

(log "test string tree\n" (str-tree->str test-str-tree))

; Returns list in the order of the tree projected from
; the top to the bottom line having the root being the
; center of the projection symmetry.
(define (tree->list-1 treeops tree)
 (define get (tree-op-get treeops))
 (define left (tree-op-get-left treeops))
 (define right (tree-op-get-right treeops))

 (define (tree2list t)
  (if (null? t) '()
   (append
    (tree2list (left t))
    (cons (get t) (tree2list (right t)))
   )
  )
 )

 (tree2list tree)
)

(log "tree->list-1 := " (tree->list-1 StringTree test-str-tree))

(define (tree->list-2 treeops tree)
 (define get (tree-op-get treeops))
 (define left (tree-op-get-left treeops))
 (define right (tree-op-get-right treeops))

 (define (copy2list t r)
  (if (null? t) r
   (copy2list (left t)
    (cons (get t) (copy2list (right t) r))
   )
  )
 )

 (copy2list tree '())
)

(log "tree->list-2 := " (tree->list-2 StringTree test-str-tree))

(define NumTree (make-tree <))
(define make-num-single (tree-op-single NumTree))
(define make-num-node (tree-op-node NumTree))
(define num-tree->str (make-tree-str-printer NumTree number->string))


;—————————————————————————————————————————————————————————————————

(define test-num-tree-1 (make-num-node 7
 (make-num-node 3 (make-num-single 1) (make-num-single 5))
 (make-num-node 9 '() (make-num-single 11))
))

(log "\ntest num tree 1\n" (num-tree->str test-num-tree-1))

(log "tree->list-1 := " (tree->list-1 NumTree test-num-tree-1))
(log "tree->list-2 := " (tree->list-2 NumTree test-num-tree-1))


;—————————————————————————————————————————————————————————————————

(define test-num-tree-2 (make-num-node 3
 (make-num-single 1)
 (make-num-node 7
  (make-num-single 5)
  (make-num-node 9 '() (make-num-single 11))
 )
))

(log "\ntest num tree 2\n" (num-tree->str test-num-tree-2))

(log "tree->list-1 := " (tree->list-1 NumTree test-num-tree-2))
(log "tree->list-2 := " (tree->list-2 NumTree test-num-tree-2))


;—————————————————————————————————————————————————————————————————

(define test-num-tree-3 (make-num-node 5
 (make-num-node 3 (make-num-single 1) '())
 (make-num-node 9 (make-num-single 7) (make-num-single 11))
))

(log "\ntest num tree 3\n" (num-tree->str test-num-tree-3))

(log "tree->list-1 := " (tree->list-1 NumTree test-num-tree-3))
(log "tree->list-2 := " (tree->list-2 NumTree test-num-tree-3))


; Conclusion: two converters produce the same resulting list.
; The first is implemented as clear recursive, the second looks
; like iterative, but is still recursive. The first traverses
; the tree from left-up-right appending items from left to
; right; the second goes right-up-left appending items from
; right to left — thus, they produce the same results.
; The first has o(n) complexity, but the second is o(n^2)
; as of append() scan-to-end implementation.
