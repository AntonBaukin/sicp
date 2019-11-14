(include "tree-test-base.scm")
(include "../3.1/random.scm")
(include "../3.1/enumerate.scm")


; Average tree size of the random tests.
(define-value-if-not 'N 20)

; Use random seed and report it to console.
(define-value-if-not 'seed (exact (truncate (current-second))))
(define random (make-random seed))

; Random N ± 20% selector.
(define random-N (make-random-in-percents random N 20))

; Random numbers of the tree in [0 .. 2N] range.
; Warning! Numbers range must be larger than N
; because we generate unique numbers.
(define random-num (make-random-in-range random 0 (* 2 N)))

(define (run-test-gen test index)
 ; Generate test tree size:
 (define n (assert-test (random-N) (list > - 0)))

 ; Generate n unique numbers to form the tree:
 (define source (assert-test
  (produce-n-unique n eq? random-num)
  (list = n length) ;<— predicate (= n (length source))
 ))

 (test index n source)
)

(define (run-test-gen-cycles T test)
 (map (curry run-test-gen test) (enumerate-n T))
 (log "Successfully completed tests: " T)
)
