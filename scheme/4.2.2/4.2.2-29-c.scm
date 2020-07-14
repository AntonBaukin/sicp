(define (log . args) (for-each display args) (newline))

(include "../3.4/ts.scm")
(include "eval-lazy.scm")
(eval-basic (debug on))

(reset-ts)

; In variant «C» we use lazy evaluator with memoize on
; to check run time of pure recursive Fibonacci.
(eval-basic
 (define (fib n)
  (cond
   ((= n 0) 0)
   ((= n 1) 1)
   (else (+ (fib (- n 2)) (fib (- n 1))))
  )
 )

 (fib 20)
)

; Memoized (this) version takes: 4.926 seconds,
; Not memoized «D» version takes: 30.236 seconds.

(log "Computed Fibonacci 20 for " (ts))
