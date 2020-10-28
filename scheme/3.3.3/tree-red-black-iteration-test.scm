; Tests number
(define T 10000)

; Random seed (or current time):
;(define seed 1)

(include "tree-red-black.scm")
(include "../2.3.3/curry.scm")

; Use red-black tree as the test target:
(define make-tree-ops (curry make-rb-tree <))

(include "../2.3.3/tree-iteration-test.scm")
