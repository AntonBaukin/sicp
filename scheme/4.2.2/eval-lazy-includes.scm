(include "../2.5.1/defined.scm")

;
; Dispatching evaluator core files:
;
(define-value-if-not 'eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.7/eval-impl-registry.scm"
  "../4.2.2/eval-impl-thunk.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.7/eval-impl-analyze.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.2.2/eval-impl-lazy.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.7/eval-impl-forms.scm"
  "../4.2.2/eval-impl-lazy-forms.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)
