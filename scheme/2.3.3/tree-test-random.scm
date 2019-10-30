(define (log . args) (for-each display args) (newline))

(include "tree.scm")
(include "tree-print.scm")
(include "../3.1/random.scm")
(include "../3.1/enumerate.scm")
(include "../3.3.2/assert.scm")


(define NumTree (make-tree <))
(define num-tree-add (tree-op-add NumTree))
(define num-tree-delete (tree-op-delete NumTree))
(define num-tree-clone (tree-op-clone NumTree))
(define num-tree->list (tree-op->list NumTree))
(define num-tree->str (make-tree->str-printer NumTree number->string))
(define num-sort (set-op-make (tree-op-Set NumTree)))


; Tests number
(define T 10000)

; Average tree size of the random tests.
(define N 20)

; Use random seed and report it to console.
(define seed (exact (truncate (current-second))))
(define random (make-random seed))

; Random N ± 20% selector.
(define random-N (make-random-in-percents random N 20))

; Random numbers of the tree in [0 .. 2N] range.
; Warning! Numbers range must be larger than N
; because we generate unique numbers.
(define random-num (make-random-in-range random 0 (* 2 N)))


(define (delete-item lst index)
 (define (next i head tail)
  (if (= i index)
   (append (reverse head) (cdr tail))
   (next (+ i 1) (cons (car tail) head) (cdr tail))
  )
 )

 (next 0 '() lst)
)

(define (run-test index)
 ; Generate test tree size:
 (define n (assert-test (random-N) (list > - 0)))

 ; Generate n unique numbers to form the tree:
 (define source (assert-test
  (produce-n-unique n eq? random-num)
  (list = n length) ;<— predicate (= n (length source))
 ))

 ; Target test tree:
 (define tree '())
 (define (add num) (set! tree (num-tree-add tree num)))

 ; Select item to delete from the list:
 (define deli ((make-random-in-range random 0 n)))
 (define delnum (list-ref source deli))

 ; Add all items to form random tree:
 (for-each add source)

 ; Are the same sorted items produced?
 (assert-equal?
  (num-sort source)
  (num-tree->list tree)

  (lambda (sorted result)
   (log "\n"
    "Seed: " seed "\n"
    "Index: " index "\n"
    "Source: " source "\n"
    "Sorted: " sorted "\n"
    "Result: " result "\n"
    (num-tree->str tree)
   )

   (error "Tree add failed!")
  )
 )

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
(map run-test (enumerate-n T))
(log "Successfully completed tests: " T)
