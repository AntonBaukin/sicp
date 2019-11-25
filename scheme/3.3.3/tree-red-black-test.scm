;(define seed 1)

(include "tree-red-black.scm")
(include "counters.scm")
(include "../2.3.3/curry.scm")
(include "../3.1/enumerate.scm")

; Use red-black tree as the test target:
(define make-tree-ops (curry make-rb-tree <))

(include "../2.3.3/tree-test-random-base.scm")


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
    (red-black? (num-tree-left node))
    (red-black? (num-tree-right node))
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

(add 10)
(log-sample "initial tree")
(assert-balanced 10 sample)

(add 4)
(log-sample "added 4")
(assert-balanced 4 sample)

(add 15)
(log-sample "added 15")
(assert-balanced 15 sample)

(add 3)
(log-sample "added 3")
(assert-balanced 3 sample)

(add 20)
(log-sample "added 20")
(assert-balanced 20 sample)

(add 23)
(log-sample "added 23")
(assert-balanced 23 sample)

(add 1)
(log-sample "added 1")
(assert-balanced 1 sample)

(add 0)
(log-sample "added 0")
(assert-balanced 0 sample)

(add 8)
(log-sample "added 8")
(assert-balanced 8 sample)

(add 6)
(log-sample "added 6")
(assert-balanced 6 sample)

(add 5)
(log-sample "added 5")
(assert-balanced 5 sample)


; Run make-from-list tests:
(run-test-gen-cycles T
 (lambda (index n source)
  (assert-balanced index (num-tree<-list source))
 )
)

(run-test-cycles T
 (lambda (index n source tree add)
  ;(log "Tree \n" (num-tree->str tree))
  (assert-balanced index tree)
 )
)
