(define (log . args) (for-each display args) (newline))

(include "../3.4/ts.scm")

(define eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.7/eval-impl-registry.scm"
  ; Herwe we use thunks without memoization:
  "../4.2.2/4.2.2-29-thunk.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.7/eval-impl-analyze.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.2.2/eval-impl-lazy.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.7/eval-impl-forms.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "eval-lazy.scm")
(eval-basic (debug on))

(reset-ts)

; In variant «D» we off memoization of thunks and
; compute the same pure recursive Fibonacci.
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

; Memoized «B» version takes: 4.926 seconds,
; Not memoized (this) version takes: 30.236 seconds.

(log "Computed Fibonacci 20 for " (ts))
