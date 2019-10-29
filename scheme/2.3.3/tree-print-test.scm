(define (log . args) (for-each display args) (newline))

(include "tree.scm")
(include "tree-print.scm")
(include "return-void.scm")
(include "../3.1/accumulator.scm")

(define StringTree (make-tree string-ci<?))
(define make-str-node (tree-op-make-node StringTree))
(define str-tree->str (make-tree->str-printer StringTree (lambda (s) s)))


(define a (make-str-node "a"))
(define b (make-str-node "b"))
(define c (make-str-node "c"))
(define d (make-str-node "d"))
(define e (make-str-node "e"))
(define f (make-str-node "f"))
(define g (make-str-node "g"))
(define ○ '())

(define tree-a-bc (make-tree-node "a" b c))
(define tree-a-○c (make-tree-node "a" ○ c))
(define tree-a-b○ (make-tree-node "a" b ○))

(define (tree-items->str tree)
 (define S (make-concatenator " "))
 ((tree-op-iter StringTree) tree (return-void S))
 (S) ;<— returns the string accumulated
)

(define (log-tree tree)
 (log
  (if (null? tree) "empty" (tree-items->str tree))
  "\n"
  (str-tree->str tree)
 )
)

(log-tree '())

(log-tree tree-a-bc)
(log-tree tree-a-○c)
(log-tree tree-a-b○)

(define tree-a-bc-defg (make-tree-node "a"
 (make-tree-node "b" d e)
 (make-tree-node "c" f g)
))

(define tree-a-b○-de (make-tree-node "a"
 (make-tree-node "b" d e)
 ○
))

(define tree-a-○c-fg (make-tree-node "a"
 ○
 (make-tree-node "c" f g)
))

(define tree-a-○c-f○ (make-tree-node "a"
 ○
 (make-tree-node "c" f ○)
))

(log-tree tree-a-bc-defg)
(log-tree tree-a-b○-de)
(log-tree tree-a-○c-fg)
(log-tree tree-a-○c-f○)
