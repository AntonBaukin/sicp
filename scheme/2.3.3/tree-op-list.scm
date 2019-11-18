
; Make-node factory takes: (item left right build),
; where: item — is the value of the node, left and
; right — are previously created nodes, or nulls,
; and support build pair.
;
; Build pair is created once for list-to-tree build.
; It's car is the level of the target node (root has
; level 0), and cdr (initially it's '()) — is left
; for the use of the make strategy.
;
; Post-build routine takes (build root-node) and
; returns the resulting root. It allows you to
; post-process the tree nodes after the build,
; i.e. mark them, set levels, re-balance...
;
(define (make-tree-op-list make-node post-build tree-ops)
 (define get-left (tree-op-left tree-ops))
 (define get-right (tree-op-right tree-ops))

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
 (define (partial-tree elements n level build)
  (if (= 0 n) (cons '() elements)
   (let* (
     (left-size (quotient (- n 1) 2))
     (left-result
      (partial-tree
       elements
       left-size
       (+ 1 level)
       build
      )
     )
     (left-tree (car left-result))
     (non-left-elements (cdr left-result))
     (right-size (- n (+ left-size 1)))   ; +1 as we take leading item
     (this-entry (car non-left-elements)) ; as this-entry of the node
     (right-result
      (partial-tree
       (cdr non-left-elements)
       right-size
       (+ 1 level)
       build
      )
     )
     (right-tree (car right-result))
     (remaining-elements (cdr right-result))
    )

    (set-car! build level)
    (cons
     (make-node this-entry left-tree right-tree build)
     remaining-elements
    )
   )
  )
 )

 (define (list->tree sequence)
  (let (
    (seq (make-set sequence))
    (build (cons 0 '()))
   )
   (post-build
    build
    (car (partial-tree seq (length seq) 0 build))
   )
  )
 )


 (list tree->list list->tree) ;<— compose list ready
)
