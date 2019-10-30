(define (log . args) (for-each display args) (newline))

(include "tree.scm")
(include "tree-print.scm")

(define NumTree (make-tree <))
(define make-num-node (tree-op-make-node NumTree))
(define num-tree-add (tree-op-add NumTree))
(define num-tree-delete (tree-op-delete NumTree))
(define num-tree->list (tree-op->list NumTree))
(define num-tree->str (make-tree->str-printer NumTree number->string))

(define sample '())

(define (add . nums)
 (for-each
  (lambda (num)
   (set! sample (num-tree-add sample num))
  )
  nums
 )
)

(define (delete . nums)
 (for-each
  (lambda (num)
   (set! sample (num-tree-delete sample num))
  )
  nums
 )
)

(define (log-sample comment)
 (log
  (num-tree->list sample)
  " <— " comment
  "\n"
  (num-tree->str sample)
 )
)

(add 10)
(log-sample "initial tree")

(add 5 3 7)
(log-sample "added 5 3 7")

(add 1 2 6)
(log-sample "added 1 2 6")

(add 15 13 12 10)
(log-sample "added 15 13 12 10")

(add 0 14 11)
(log-sample "added 0 14 11")

(delete 11 6 0 7)
(log-sample "deleted leafs 11 6 0 7")

(delete 1 3 15)
(log-sample "deleted singles 1 3 15")

(add 1 0 11 15 17 4 3)
(log-sample "added 1 0 11 15 17 4 3")

(delete 13)
(log-sample "deleted double 13")

(delete 2)
(log-sample "deleted double 2")

(delete 10)
(log-sample "deleted double root 10")
