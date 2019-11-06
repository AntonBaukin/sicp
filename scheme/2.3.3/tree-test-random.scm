(include "tree-test-random-base.scm")

; Tests number
(define T 10000)

(define (test index n source tree add)
 ; Select item to delete from the list:
 (define deli ((make-random-in-range random 0 n)))
 (define delnum (list-ref source deli))

 (let ((tree2 (num-tree-clone tree)))
  (set! tree2 (num-tree-delete tree2 delnum))

  (assert-equal?
   (num-sort (delete-item source deli))
   (num-tree->list tree2)

   (lambda (sorted result)
    (log "\n"
     "Seed: " seed "\n"
     "Index: " index "\n"
     "Deleting item: " delnum "\n"
     "Source tree: " sorted "\n"
     (num-tree->str tree) "\n"
     "Result tree: " result "\n"
     (num-tree->str tree2) "\n"
    )

    (error "Tree delete failed!")
   )
  )
 )
)

; Run the tests:
(run-test-cycles T test)
