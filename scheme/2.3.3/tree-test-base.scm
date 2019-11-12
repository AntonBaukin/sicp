(define (log . args) (for-each display args) (newline))

(include "curry.scm")
(include "tree.scm")
(include "tree-print.scm")
(include "../3.3.2/assert.scm")

(define NumTree (make-tree <))
(define make-num-node (tree-op-make-node NumTree))
(define num-tree-get (tree-op-get NumTree))
(define num-tree-add (tree-op-add NumTree))
(define num-tree-delete (tree-op-delete NumTree))
(define num-tree-clone (tree-op-clone NumTree))
(define num-tree->list (tree-op->list NumTree))
(define num-tree->str (make-tree->str-printer NumTree number->string))
(define num-tree-smaller? (tree-op-smaller? NumTree))
(define num-sort (set-op-make (tree-op-Set NumTree)))

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

(define sample '())