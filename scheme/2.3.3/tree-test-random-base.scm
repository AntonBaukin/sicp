(include "tree-test-base.scm")
(include "../3.1/random.scm")
(include "../3.1/enumerate.scm")


; Average tree size of the random tests.
(define N 20)

; Use random seed and report it to console.
(define seed 1);(exact (truncate (current-second))))
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

(define (run-test test index)
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

 (test index n source tree add)
)

(define (run-test-cycles T test)
 (map (curry run-test test) (enumerate-n T))
 (log "Successfully completed tests: " T)
)
