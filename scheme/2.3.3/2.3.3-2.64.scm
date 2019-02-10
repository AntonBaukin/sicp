(define (log . args) (for-each display args) (newline))

(include "2.3.3-tree.scm")
(include "2.3.3-tree-print.scm")

(define (list->tree treeops list-elements)
 (define (make-node v l r)
  ; (log "node " v " " l " " r)
  ((tree-op-node treeops) v l r)
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

 (car (partial-tree list-elements (length list-elements)))
)

(define NumTree (make-tree <))
(define num-tree->str (make-tree-str-printer NumTree number->string))

(log "(1 3 5 7 9 11) as balanced tree\n"
 (num-tree->str (list->tree NumTree '(1 3 5 7 9 11)))
)
