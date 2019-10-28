
(define (make-tree-op-list make-node tree-ops)
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
     (make-node this-entry left-tree right-tree)
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


 (list tree->list list->tree) ;<— compose list ready
)
