(define (log . args) (for-each display args) (newline))

(include "tree.scm")
(include "tree-print.scm")

(define (tree->list treeops tree)
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

(define (list->tree treeops list-elements)
 (define make-node (tree-op-node treeops))

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

(define (union-set treeops set-tree-a set-tree-b)
 (define smaller? (tree-op-smaller? treeops))

 ; Implementation from «sorted-set.scm» takes o(n + m)
 (define (union seta setb)
  (cond
   ((null? seta) setb)
   ((null? setb) seta)

   ((smaller? (car seta) (car setb))
    (cons (car seta) (union (cdr seta) setb))
   )

   ((smaller? (car setb) (car seta))
    (cons (car setb) (union seta (cdr setb)))
   )

   ; not < and not > means = to take single of equal items
   (else (cons (car seta) (union (cdr seta) (cdr setb))))
  )
 )

 ; tree->list takes o(n) + o(m); list->tree takes o(n + m)
 (list->tree treeops (union
  (tree->list treeops set-tree-a)
  (tree->list treeops set-tree-b)
 ))
)

(define (intersect-set treeops set-tree-a set-tree-b)
 (define smaller? (tree-op-smaller? treeops))

 ; Implementation from «sorted-set.scm» takes o(n + m)
 (define (intersect seta setb)
  (cond
   ((null? seta) '())
   ((null? setb) '())

   ((smaller? (car seta) (car setb))
    (intersect (cdr seta) setb)
   )

   ((smaller? (car setb) (car seta))
    (intersect seta (cdr setb))
   )

   ; not < and not > means = to take single of equal items
   (else (cons (car seta) (intersect (cdr seta) (cdr setb))))
  )
 )

 (list->tree treeops (intersect
  (tree->list treeops set-tree-a)
  (tree->list treeops set-tree-b)
 ))
)

(define NumTree (make-tree <))
(define num-tree->str (make-tree-str-printer NumTree number->string))
(define (num-tree->list l) (tree->list NumTree l))
(define (num-list->tree l) (list->tree NumTree l))

(define A (num-list->tree '(1 3 4 5 7 8 9)))
(define B (num-list->tree '(2 3 4 6 7 8 10 11)))

(log "Set A " (num-tree->list A) " as balanced tree\n" (num-tree->str A))
(newline)
(log "Set B " (num-tree->list B) " as balanced tree\n" (num-tree->str B))
(newline)

(define A∪B (union-set NumTree A B))
(define A∩B (intersect-set NumTree A B))

(log "A ∪ B = " (num-tree->list A∪B) "\n" (num-tree->str A∪B))
(log "A ∩ B = " (num-tree->list A∩B) "\n" (num-tree->str A∩B))
