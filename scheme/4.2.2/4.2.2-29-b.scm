(define (log . args) (for-each display args) (newline))

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

; In variant «B» we use lazy evaluator without thunks memoization.
(eval-basic
 (define count 0)

 (define (id x)
  (debug log "... inc count = " count)
  (set! count (+ count 1)) ;<— side effect
  x
 )

 (define (square x) (* x x))

 (debug log "In variant «B» we use non-memoizing lazy evaluator:")
 (debug log "(square (id 10)) = " (square (id 10)))
 (debug log "count = " count)
 ;
 ; ... inc count = 0
 ; (square (id 10)) = 100
 ; count = 2
 ;
 ; When «x» argument is passed to primary «*» second time,
 ; it's resolved again, and second side-effect of count
 ; increment is evaluated.
 ;
 ; In test «A» with memoization on we've got 1,
 ; without memoization it's 2, — as predicted.
 ;
)
