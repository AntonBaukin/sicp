;(define seed 1)

(include "tree-red-black.scm")
(include "../2.3.3/curry.scm")
(include "../3.1/enumerate.scm")

; Use red-black tree as the test target:
(define make-tree-ops (curry make-rb-tree <))

(include "../2.3.3/tree-test-random-run.scm")


; Tests number
(define T 10000)

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

(define (assert-balanced index tree)
 (define (red? node) (eq? 'red (cadr node)))

 (define (black? node)
  (or
   (null? node)
   (eq? 'black (cadr node))
  )
 )

 (define (black-length sel node)
  (if (null? node) 0
   (+
    (if (black? node) 1 0)
    (sel
     (black-length sel (num-tree-left node))
     (black-length sel (num-tree-right node))
    )
   )
  )
 )

 (define min-length (black-length min tree))
 (define max-length (black-length max tree))

 (define (red-black? node)
  (if (null? node) #t
   (and
    (or
     (black? node)
     (and
      (red? node)
      (black? (num-tree-left node))
      (black? (num-tree-right node))
     )
    )
    (num-tree-left node)
    (num-tree-right node)
   )
  )
 )

 (assert-true?
  (black? tree)
  (lambda ()
   (log "\n"
    "Seed: " seed "\n"
    "Index: " index "\n"
    "Failed tree:\n"
    (num-tree->str tree) "\n"
   )

   (error "Red-black tree's root is not black!")
  )
 )

 (assert-true?
  (= max-length min-length)

  (lambda ()
   (log "\n"
    "Seed: " seed "\n"
    "Index: " index "\n"
    "Min black length: " min-length "\n"
    "Max black length: " max-length "\n"
    "Failed tree:\n"
    (num-tree->str tree) "\n"
   )

   (error "Red-black black-length mismatch!")
  )
 )

 (assert-true?
  (red-black? tree)

  (lambda ()
   (log "\n"
    "Seed: " seed "\n"
    "Index: " index "\n"
    "Failed tree:\n"
    (num-tree->str tree) "\n"
   )

   (error "Red-black red-child test!")
  )
 )
)

; Run make-from-list tests:
(run-test-gen-cycles T
 (lambda (index n source)
  (assert-balanced index (num-tree<-list source))
 )
)

;(define sample (num-tree<-list (enumerate-n 5)))
;(log-sample "initial tree")
;(assert-balanced 0 sample)
