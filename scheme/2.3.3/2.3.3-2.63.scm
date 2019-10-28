(define (log . args) (for-each display args) (newline))

(include "tree.scm")
(include "tree-print.scm")

(define StringTree (make-tree string-ci<?))
(define sleaf (tree-op-make-root StringTree))
(define str-tree->str (make-tree->str-printer StringTree (lambda (s) s)))


(define test-str-tree 
 (make-tree-node "a"
  (make-tree-node "b" (sleaf "d") (sleaf "e"))
  (make-tree-node "c" (sleaf "f") (sleaf "g"))
 )
)

(log "test string tree\n" (str-tree->str test-str-tree))

; Returns list in the order of the tree projected from
; the top to the bottom line having the root being the
; center of the projection symmetry.
(define (tree->list-1 tree-ops tree)
 (define get (tree-op-get tree-ops))
 (define left (tree-op-left tree-ops))
 (define right (tree-op-right tree-ops))

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

(define (tree->list-2 tree-ops tree)
 (define get (tree-op-get tree-ops))
 (define left (tree-op-left tree-ops))
 (define right (tree-op-right tree-ops))

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
(define nleaf (tree-op-make-root NumTree))
(define num-tree->str (make-tree->str-printer NumTree number->string))


;—————————————————————————————————————————————————————————————————

(define test-num-tree-1
 (make-tree-node 7
  (make-tree-node 3 (nleaf 1) (nleaf 5))
  (make-tree-node 9 '() (nleaf 11))
 )
)

(log "\ntest num tree 1\n" (num-tree->str test-num-tree-1))

(log "tree->list-1 := " (tree->list-1 NumTree test-num-tree-1))
(log "tree->list-2 := " (tree->list-2 NumTree test-num-tree-1))


;—————————————————————————————————————————————————————————————————

(define test-num-tree-2
 (make-tree-node 3
  (nleaf 1)
  (make-tree-node 7
   (nleaf 5)
   (make-tree-node 9 '() (nleaf 11))
  )
 )
)

(log "\ntest num tree 2\n" (num-tree->str test-num-tree-2))

(log "tree->list-1 := " (tree->list-1 NumTree test-num-tree-2))
(log "tree->list-2 := " (tree->list-2 NumTree test-num-tree-2))


;—————————————————————————————————————————————————————————————————

(define test-num-tree-3
 (make-tree-node 5
  (make-tree-node 3 (nleaf 1) '())
  (make-tree-node 9 (nleaf 7) (nleaf 11))
 )
)

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
