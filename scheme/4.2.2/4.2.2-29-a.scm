(define (log . args) (for-each display args) (newline))

(include "eval-lazy.scm")
(eval-basic (debug on))

; In variant «A» we use as-is our lazy evaluator (with memoize on).
(eval-basic
 (define count 0)

 (define (id x)
  (debug log "... inc count = " count)
  (set! count (+ count 1)) ;<— side effect
  x
 )

 (define (square x) (* x x))

 (debug log "In variant «A» we use memoizing lazy evaluator:")
 (debug log "(square (id 10)) = " (square (id 10)))
 (debug log "count = " count)
 ;
 ; ... inc count = 0
 ; (square (id 10)) = 100
 ; count = 1
 ;
 ; When «x» argument is passed to primary «*» second time,
 ; it's resolved from meoized thunk, and second side-effect
 ; of count increment is not evaluated.
 ;
 ; In test «B» with memoization off we expect it to be 2...
 ;
)
