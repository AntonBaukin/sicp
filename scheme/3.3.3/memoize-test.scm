(include "../3.3.2/assert.scm")
(include "../3.1/enumerate.scm")
(include "../3.3.3/tree-red-black.scm")
(include "../2.5.3/index-tree.scm")
(include "table.scm")
(include "table-cache.scm")
(include "memoize.scm")

(define (log . args) (for-each display args) (newline))

(define (test-mem N)
 (define stage 0)
 (define imake 0)

 (define (test i)
  (cond
   ; We invoke this function N times and cache the values:
   ((= 0 stage)
    (assert-eq? i imake "Wrong make of stage 0!")
    (set! imake (+ imake 1))
   )

   ; Values of previous stage must be cached:
   ((= 1 stage)
    (error "Lost memoized value of stage 0!" i)
   )

   ; We produce next N-values:
   ((= 2 stage)
    (assert-eq? i imake "Wrong make of stage 2!")
    (set! imake (+ imake 1))
   )

   ((= 3 stage)
    (error "Lost memoized value of stage 3!" i)
   )

   ; When we produce next N-values, previously
   ; cached must be pruned:
   ((= 4 stage)
    (assert-eq? (+ (* 2 N) i) imake "Wrong make of stage 4!")
    (set! imake (+ imake 1))
   )

   ((= 5 stage)
    (error "Lost memoized value of stage 5!" i)
   )
   
   (else (error "Wrong test stage!"))
  )

  (number->string i) ;<— resulting value
 )

 (define test-mem (memoize N test))

 (define (test-run i)
  (assert-equal? (number->string i) (test-mem i)
   "Wrong resulting value of memoized test!"
  )
 )

 ; Values [0 .. N-1]
 (define nN (enumerate-n N))

 ; Values [N .. 2N-1]
 (define n2N (enumerate-range N (- (* 2 N) 1)))

 (for-each test-run nN)

 (set! stage 1)
 (for-each test-run nN)

 (set! stage 2)
 (for-each test-run n2N)

 (set! stage 3)
 (for-each test-run n2N)

 (set! stage 4)
 (for-each test-run nN)

 (set! stage 5)
 (for-each test-run nN)
)

; Run tests for limits [1 .. 100]:
(map test-mem (enumerate-range 1 100))
