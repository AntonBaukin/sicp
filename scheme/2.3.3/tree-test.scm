(define (log . args) (for-each display args) (newline))

(include "tree.scm")
(include "tree-print.scm")

(define StringTree (make-tree string-ci<?))
(define make-str-single (tree-op-single StringTree))
(define make-str-node (tree-op-node StringTree))
(define str-tree->str (make-tree->str-printer StringTree (lambda (s) s)))

(define a (make-str-single "a"))
(define b (make-str-single "b"))
(define c (make-str-single "c"))
(define d (make-str-single "d"))
(define e (make-str-single "e"))
(define f (make-str-single "f"))
(define g (make-str-single "g"))
(define ○ '())

(define tree-a-bc (make-str-node "a" b c))
(define tree-a-○c (make-str-node "a" ○ c))
(define tree-a-b○ (make-str-node "a" b ○))

(define (tree-items->str tree)
 (define result "")

 ((tree-op-iter StringTree) tree
  (lambda (s)
   (set! result (string-append result s))
   void
  )
 )

 result
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

(define tree-a-bc-defg (make-str-node "a"
 (make-str-node "b" d e)
 (make-str-node "c" f g)
))

(define tree-a-b○-de (make-str-node "a"
 (make-str-node "b" d e)
 ○
))

(define tree-a-○c-fg (make-str-node "a"
 ○
 (make-str-node "c" f g)
))

(define tree-a-○c-f○ (make-str-node "a"
 ○
 (make-str-node "c" f ○)
))

(log-tree tree-a-bc-defg)
(log-tree tree-a-b○-de)
(log-tree tree-a-○c-fg)
(log-tree tree-a-○c-f○)
