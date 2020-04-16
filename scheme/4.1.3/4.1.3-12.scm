(define (log . args) (for-each display args) (newline))

(define eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  ; Our replacements for generic lookup routines:
  "../4.1.3/4.1.3-12-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.2/eval-impl-disp.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.2/eval-impl-forms.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "../4.1.2/eval-disp.scm")

; Now we run overall tests:
(include "../3.3.2/assert.scm")
(include "../4.1.1/eval-test-items.scm")
