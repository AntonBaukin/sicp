(define (log . args) (for-each display args) (newline))

; Eve's variant of variables resolve involves
; promises — see the comment below.

(define eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.2/eval-impl-disp.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.6/4.1.6-19-env.scm"
  "../4.1.6/4.1.6-19-impl.scm"
  "../4.1.2/eval-impl-forms.scm"
  "../4.1.6/eval-impl-forms.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

; Enable debug mode:
(eval-basic (debug on))

(assert-eq? 20
 (eval-basic
  (let ((a 1))
   (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b)

    ; This body is translated into the following:
    ;
    ; (define b (cons 'promised (delay (+ a x))))
    ; (define a (cons 'promised (delay 5)))
    ; (+ a b)
    ;
    ; Promised tag is checked when resolving a variable.
    ; (Unassigned marker is not used any more.)
    ;
    ; As we delay the evaluation, variable «a» is resolved
    ; to values 5 (not 1) ar the force time.
   )
   (f 10)
  )
 )
)
