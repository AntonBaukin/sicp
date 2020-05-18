(define (log . args) (for-each display args) (newline))

; The same effect of «letrec» implemented
; as a transformation to «let».

(define eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.2/eval-impl-disp.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.6/4.1.6-16-env.scm"
  "../4.1.2/eval-impl-forms.scm"
  "../4.1.6/eval-impl-forms.scm"
  "../4.1.6/4.1.6-20-impl.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")


(assert-eq? 3628800
 (eval-basic
  (letrec ((fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))))
   (fact 10)
  )
 )
)
