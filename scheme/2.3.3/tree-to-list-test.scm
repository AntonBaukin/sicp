(include "tree-test-random-run.scm")

; Tests number
(define T 10000)

(define (test index n source)
 (define tree (num-tree<-list source))

 (assert-equal?
  (num-sort source)
  (num-tree->list tree)

  (lambda (sorted back)
   (log "\n"
    "Seed: " seed "\n"
    "Index: " index "\n"
    "Sorted list: " sorted "\n"
    "Back list: " back "\n"
    (num-tree->str tree) "\n"
   )

   (error "List to tree and back failed!")
  )
 )
)

; Run the tests:
(run-test-gen-cycles T test)
