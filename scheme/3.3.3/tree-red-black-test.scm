; Tests number
(define T 10000)

;(define seed 1)

(include "tree-red-black.scm")
(include "../2.3.3/curry.scm")
(include "../3.1/enumerate.scm")

; Use red-black tree as the test target:
(define make-tree-ops (curry make-rb-tree <))

(include "../2.3.3/tree-test-random-base.scm")
(include "../2.3.3/tree-util-walk.scm")
(include "../2.3.3/tree-util-iter.scm")
(include "../2.3.3/tree-util-leafs.scm")


(define num-tree-get-leafs (make-tree-get-leafs NumTree))

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

; Returns list of children that have single child.
(define (find-soles tree)
 (define iter (make-tree-util-iter NumTree))
 (define result '())

 (define (sole? node)
  (define l? (null? (num-tree-left node)))
  (define r? (null? (num-tree-right node)))
  (or (and l? (not r?)) (and (not l?) r?))
 )

 (iter tree
  (lambda (node stack)
   (cond
    ((sole? node)
     (set! result (cons node result))
     void ;<— request the next
    )
    (else void)
   )
  )
 )

 (reverse result)
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
  (if (null? node) 1
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

(define (assert-equal-tree index source tree)
 (assert-equal?
  (num-sort source)
  (num-tree->list tree)

  (lambda (sorted back)
   (log "\n"
    "Seed: " seed "\n"
    "Index: " index "\n"
    "Sorted list: " sorted "\n"
    "Tree list: " back "\n"
    (num-tree->str tree) "\n"
   )

   (error "Tree equality had failed!")
  )
 )
)


; Test creating tree from list.
(run-test-gen-cycles T
 (lambda (index n source)
  (define tree (num-tree<-list source))

  (assert-equal-tree index source tree)
  (assert-balanced index tree)
 )
)

(random-reset)

; Set failed index to see i-logging.
;(set! log-index error-index)


; Test creating tree by adding items, then remove a leaf node.
(run-test-cycles T
 (lambda (index n source tree add)
  (define leafs (map caar (num-tree-get-leafs tree)))
  (define deli ((make-random-in-range random 0 (length leafs))))
  (define delnum (list-ref leafs deli))
  (define sourcex (delete-value source delnum))

  (assert-equal-tree index source tree)
  (assert-balanced index tree)

  (ilog "Removing leaf " delnum "\n" (num-tree->str tree))
  (set! tree (num-tree-delete tree delnum))

  (assert-equal-tree index sourcex tree)
  (assert-balanced index tree)
 )
)

(random-reset)

; Set failed index to see i-logging.
;(set! log-index error-index)


; Create tree by adding items, then remove a sole node.
(run-test-cycles T
 (lambda (index n source tree add)
  (define soles (map car (find-soles tree)))

  (assert-equal-tree index source tree)
  (assert-balanced index tree)

  ; Has no sole nodes? Just skip this test:
  (if (null? soles) void
   (let* (
     (deli ((make-random-in-range random 0 (length soles))))
     (delnum (list-ref soles deli))
     (sourcex (delete-value source delnum))
    )

    (ilog "Removing sole " delnum "\n" (num-tree->str tree))
    (set! tree (num-tree-delete tree delnum))

    (assert-equal-tree index sourcex tree)
    (assert-balanced index tree)
   )
  )
 )
)

; Test creating tree by adding items, then remove node.
(run-test-cycles T
 (lambda (index n source tree add)
  (define deli ((make-random-in-range random 0 (length source))))
  (define delnum (list-ref source deli))
  (define sourcex (delete-value source delnum))

  (assert-equal-tree index source tree)
  (assert-balanced index tree)

  (ilog "Removing " delnum "\n" (num-tree->str tree))
  (set! tree (num-tree-delete tree delnum))

  (assert-equal-tree index sourcex tree)
  (assert-balanced index tree)
 )
)
