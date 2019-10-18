(define (log . args) (for-each display args) (newline))

(include "tree.scm")
(include "tree-print.scm")

(define (union-set tree-ops set-tree-a set-tree-b)
 (define smaller? (tree-op-smaller? tree-ops))
 (define tree->list (tree-op->list tree-ops))
 (define list->tree (tree-op<-list tree-ops))

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
 (list->tree (union
  (tree->list set-tree-a)
  (tree->list set-tree-b)
 ))
)

(define (intersect-set tree-ops set-tree-a set-tree-b)
 (define smaller? (tree-op-smaller? tree-ops))
 (define tree->list (tree-op->list tree-ops))
 (define list->tree (tree-op<-list tree-ops))

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

 (list->tree (intersect
  (tree->list set-tree-a)
  (tree->list set-tree-b)
 ))
)

(define NumTree (make-tree <))
(define num-tree->str (make-tree->str-printer NumTree number->string))
(define num-tree->list (tree-op->list NumTree))
(define num-list->tree (tree-op<-list NumTree))

(define A (num-list->tree '(5 8 1 4 7 3 9)))
(define B (num-list->tree '(11 2 6 7 4 8 10 3)))

(log "Set A " (num-tree->list A) " as balanced tree\n" (num-tree->str A))
(newline)
(log "Set B " (num-tree->list B) " as balanced tree\n" (num-tree->str B))
(newline)

(define A∪B (union-set NumTree A B))
(define A∩B (intersect-set NumTree A B))

(log "A ∪ B = " (num-tree->list A∪B) "\n" (num-tree->str A∪B))
(log "A ∩ B = " (num-tree->list A∩B) "\n" (num-tree->str A∩B))
