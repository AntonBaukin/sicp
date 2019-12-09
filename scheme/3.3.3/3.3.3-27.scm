(include "../3.3.3/tree-red-black.scm")
(include "../2.5.3/index-tree.scm")
(include "table.scm")
(include "table-cache.scm")
(include "memoize.scm")

(define (log . args) (for-each display args) (newline))

; Current time in milliseconds:
(define (time-ms) (exact (truncate (* 1000 (current-second)))))

(define (test f n what)
 (define t (time-ms))
 (define v (f n))
 (log "Completed " what " for N = " n " in " (- (time-ms) t))
)

(define (fib n)
 (cond
  ((= n 0) 0)
  ((= n 1) 1)
  (else
   (+
    (fib (- n 2))
    (fib (- n 1))
   )
  )
 )
)

(define fibm (memoize 2 (lambda (n)
 (cond
  ((= n 0) 0)
  ((= n 1) 1)
  (else
   (+
    (fibm (- n 2))
    (fibm (- n 1))
   )
  )
 )
)))

; Iterative fib from task «1.2.4-1.19.scm».
(define (fibi n)
 (define (next i a b)
  (if (= i n)
   (cons a b)
   (next (+ i 1) b (+ a b))
  )
 )

 (cond
  ((= n 0) 0)
  ((= n 1) 1)
  (else (next 1 0 1))
 )
)

(test fib 32 "recursive fib")
(test fibm 32 "memoized fib")

(test fibi 100000 "iterative fib")
(test fibm 100000 "memoized fib")

; Sample outputs:
;
; Completed recursive fib for N = 32 in 3710
; Completed memoized fib for N = 32 in 3
; Completed iterative fib for N = 100000 in 463
; Completed memoized fib for N = 100000 in 9308
;
; As we see, memoized version is 20 times slower than
; plain iterative — as it has noticable overhead
; for tree cache lookup and prune-add ops.
