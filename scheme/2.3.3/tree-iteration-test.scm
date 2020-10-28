(include "../2.5.1/defined.scm")

; Tests number:
(define-value-if-not 'T 10000)

; Random seed (or current time):
; (define seed 1)

(include "tree-test-random-run.scm")

(define (num-tree-iter->list tree)
 (define result '())

 (num-tree-iter tree
  (lambda (item)
   (set! result (cons item result))
   void
  )
 )

 (reverse result)
)

(define (num-tree-iterator->list tree)
 (define it (num-tree-iterator tree))

 (define (next result)
  (define n (it))

  (if (null? n)
   (reverse result)
   (next
    (cons
     (num-tree-get n)
     result
    )
   )
  )
 )

 (next '())
)

(define (test index n source)
 (define tree (num-tree<-list source))
 (define sorted (num-sort source))

 (assert-equal?
  sorted
  (num-tree-iter->list tree)

  (lambda (sorted back)
   (log "\n"
    "Seed: " seed "\n"
    "Index: " index "\n"
    "Sorted list: " sorted "\n"
    "Back list: " back "\n"
    (num-tree->str tree) "\n"
   )

   (error "Tree visitor iteration failed!")
  )
 )

 (assert-equal?
  sorted
  (num-tree-iterator->list tree)

  (lambda (sorted back)
   (log "\n"
    "Seed: " seed "\n"
    "Index: " index "\n"
    "Sorted list: " sorted "\n"
    "Back list: " back "\n"
    (num-tree->str tree) "\n"
   )

   (error "Tree iteration failed!")
  )
 )
)

; Run the tests:
(run-test-gen-cycles T test)
