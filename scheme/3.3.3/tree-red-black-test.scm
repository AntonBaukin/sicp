(include "tree-red-black.scm")
(include "../2.3.3/curry.scm")
(include "../3.1/enumerate.scm")

; Use red-black tree as the test target:
(define make-tree-ops (curry make-rb-tree <))

(include "../2.3.3/tree-test-random-run.scm")


; Use raw printer to display the nodes color:
(define num-tree->str
 (make-tree->str-printer-raw
  NumTree
  (lambda (node)
   (string-append
    "[" (if (eq? 'black (cadr node)) "B" "R") "] "
    (number->string (car node))
   )
  )
 )
)

(define (assert-balanced tree)
 (define (black? node) (eq? 'black (cadr node)))
 (define (red? node) (eq? 'red (cadr node)))

 (assert-true?
  (red? tree)
  (lambda ()
   (log "\n"
    "Seed: " seed "\n"
    "Failed tree:\n"
    (num-tree->str tree) "\n"
   )

   (error "Red-black tree's root is red!")
  )
 )
)


(define sample (num-tree<-list (enumerate-n 10)))
(log-sample "initial tree")
(assert-balanced sample)
