(define (log . args) (for-each display args) (newline))

(include "tree.scm")
(include "tree-print.scm")
(include "return-void.scm")
(include "../3.3.2/assert.scm")
(include "../3.1/accumulator.scm")


(define NumTree (make-tree <))
(define make-num-node (tree-op-make-node NumTree))
(define num-tree-add (tree-op-add NumTree))
(define num-tree->str (make-tree->str-printer NumTree number->string))

(define (tree-items->str tree)
 (define S (make-concatenator " " number->string))
 ((tree-op-iter NumTree) tree (return-void S))
 (S) ;<— returns the string accumulated
)

(define sample (make-num-node 10))

(define (add . nums)
 (for-each
  (lambda (num)
   (set! sample (num-tree-add sample num))
  )
  nums
 )
)

(define (log-sample comment)
 (log
  (tree-items->str sample)
  " <— " comment
  "\n"
  (num-tree->str sample)
 )
)

(log-sample "initial tree")

(add 5 3 7)
(log-sample "added 5 3 7")

(add 1 2 6)
(log-sample "added 1 2 6")

(add 15 13 12 10)
(log-sample "added 15 13 12 10")

(add 0 14 11)
(log-sample "added 0 14 11")
