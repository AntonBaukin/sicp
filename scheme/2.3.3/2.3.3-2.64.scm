(define (log . args) (for-each display args) (newline))

(include "tree.scm")
(include "tree-print.scm")

(define NumTree (make-tree <))
(define num-tree->str (make-tree-str-printer NumTree number->string))
(define num-list->tree (tree-op<-list NumTree))

(log "(11 1 7 5 9 3) as balanced tree\n"
 (num-tree->str (num-list->tree '(11 1 7 5 9 3)))
)
