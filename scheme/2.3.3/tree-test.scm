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

(log "a-bc\n" (str-tree->str tree-a-bc))
(log "a-○c\n" (str-tree->str tree-a-○c))
(log "a-b○\n" (str-tree->str tree-a-b○))

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

(log "a-bc-defg\n" (str-tree->str tree-a-bc-defg))
(log "a-b○-de\n" (str-tree->str tree-a-b○-de))
(log "a-○c-fg\n" (str-tree->str tree-a-○c-fg))
(log "a-○c-f○\n" (str-tree->str tree-a-○c-f○))
