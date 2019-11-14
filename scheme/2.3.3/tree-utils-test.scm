(include "tree-test-random-base.scm")
(include "tree-util-walk.scm")
(include "tree-util-max.scm")
(include "tree-util-min.scm")
(include "tree-util-prev.scm")
(include "tree-util-next.scm")
(include "tree-util-iter.scm")


; Tests number
(define T 10000)

(define tree-min (make-tree-get-min NumTree))
(define tree-max (make-tree-get-max NumTree))
(define tree-prev (make-tree-get-prev NumTree))
(define tree-next (make-tree-get-next NumTree))
(define tree-iter (make-tree-util-iter NumTree))

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

(define (is-correct-stack tree node stack)
 (define get-left (tree-op-left NumTree))
 (define get-right (tree-op-right NumTree))

 (define (is-parent node parent)
  (or
   (eq? node (get-left parent))
   (eq? node (get-right parent))
  )
 )

 (define (trace-up node stack)
  (if (null? stack) node
   (if (is-parent node (car stack))
    (trace-up (car stack) (cdr stack))
    void 
   )
  )
 )

 (eq? tree (trace-up node stack))
)

(define (test index n source tree add)
 (define sorted (num-sort source))
 (define all-nodes '()) ;<— will collect them

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

 ; Run prev-next tests for each target number in [0; 2N]:
 (map test-prev (enumerate-n (+ 1 (* 2 N))))

 ; Run test for extended in-order iterator:
 (tree-iter tree
  (lambda (node stack)
   (set! all-nodes (cons node all-nodes))

   (assert-true?
    (is-correct-stack tree node stack)
    (lambda ()
     (log "\n"
      "Seed: " seed "\n"
      "Index: " index "\n"
      "Node: " node "\n"
      "Stack: " stack "\n"
      "Tree: " sorted "\n"
      (num-tree->str tree) "\n"
     )

     (error "Tree util iteration stack check failed!")
    )
   )

   void ;<— always continue the iteration
  )
 )

 (assert-equal?
  sorted
  ; Take values from the collected nodes:
  (map num-tree-get (reverse all-nodes))

  (lambda (sorted collected)
    (log "\n"
     "Seed: " seed "\n"
     "Index: " index "\n"
     "Expected items: " expected "\n"
     "Collected items: " collected "\n"
     "Tree: " sorted "\n"
     (num-tree->str tree) "\n"
    )

    (error "Tree util iteration order failed!")
   )
 )
)

; Run the tests:
(run-test-cycles T test)
