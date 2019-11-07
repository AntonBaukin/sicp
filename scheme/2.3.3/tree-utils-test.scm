(include "tree-test-random-base.scm")
(include "tree-util-walk.scm")
(include "tree-util-max.scm")
(include "tree-util-min.scm")
(include "tree-util-prev.scm")
(include "tree-util-next.scm")


; Tests number
(define T 10000)

(define tree-min (make-tree-get-min NumTree))
(define tree-max (make-tree-get-max NumTree))
(define tree-prev (make-tree-get-prev NumTree))
(define tree-next (make-tree-get-next NumTree))

(define (find-smaller res sorted item)
 (cond
  ((null? sorted) res)
  ((num-tree-smaller? (car sorted) item)
   (find-smaller (car sorted) (cdr sorted) item)
  )
  (else res)
 )
)

(define (find-greater sorted item)
 (cond
  ((null? sorted) '())
  ((num-tree-smaller? item (car sorted))
   (car sorted)
  )
  (else (find-greater (cdr sorted) item))
 )
)

(define (test index n source tree add)
 (define sorted (num-sort source))

 ; Sub-test that checks prev and next implementation.
 (define (test-prev num)
  (assert-eq?
   (find-smaller '() sorted num)
   (tree-prev tree num)

   (lambda (expected found)
     (log "\n"
      "Seed: " seed "\n"
      "Index: " index "\n"
      "Prev target item: " num "\n"
      "Expected prev: " expected "\n"
      "Found prev: " found "\n"
      "Tree: " sorted "\n"
      (num-tree->str tree) "\n"
     )

     (error "Tree util get-prev failed!")
    )
  )

  (assert-eq?
   (find-greater sorted num)
   (tree-next tree num)

   (lambda (expected found)
     (log "\n"
      "Seed: " seed "\n"
      "Index: " index "\n"
      "Next target item: " num "\n"
      "Expected next: " expected "\n"
      "Found next: " found "\n"
      "Tree: " sorted "\n"
      (num-tree->str tree) "\n"
     )

     (error "Tree util get-next failed!")
    )
  )
 )

 (assert-eq?
  (car sorted)
  (tree-min tree)

  (lambda (expected found)
    (log "\n"
     "Seed: " seed "\n"
     "Index: " index "\n"
     "Expected min: " expected "\n"
     "Found min: " found "\n"
     "Tree: " sorted "\n"
     (num-tree->str tree) "\n"
    )

    (error "Tree util get-min failed!")
   )
 )

 (assert-eq?
  (car (reverse sorted))
  (tree-max tree)

  (lambda (expected found)
    (log "\n"
     "Seed: " seed "\n"
     "Index: " index "\n"
     "Expected max: " expected "\n"
     "Found max: " found "\n"
     "Tree: " sorted "\n"
     (num-tree->str tree) "\n"
    )

    (error "Tree util get-max failed!")
   )
 )

 (map test-prev (enumerate-n (+ 1 (* 2 N))))
)

; Run the tests:
(run-test-cycles T test)
