(define (log . args) (for-each display args) (newline))

(define eval-basic-includes
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
  ; Here we replace standard quotation form:
  "../4.2.3/4.2.3-33-forms.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)


(include "streams-lazy.scm")
(include "../4.1.6/eval-assert.scm")

(eval-basic (debug on))

(eval-basic
 (define five '(1 2 3 4 5))

 (assert-true? (list? five))
 (assert-eq? 5 (length five))
)
