(include "../2.5.1/defined.scm")

;
; Basic evaluator core files:
;
(define-value-if-not 'eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.1/eval-impl-basic.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.1/eval-impl-set.scm"
 )
)
