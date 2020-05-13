(define (log . args) (for-each display args) (newline))

; Lisa's version copies variable resolve behaviour
; from task 4.16. We expect an error on «a» here.

(define eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.2/eval-impl-disp.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.6/4.1.6-16-env.scm"
  "../4.1.6/4.1.6-16-impl.scm"
  "../4.1.2/eval-impl-forms.scm"
  "../4.1.6/eval-impl-forms.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

(assert-error
 (lambda ()
  (eval-basic
   (let ((a 1))
    (define (f x)
     (define b (+ a x))
     (define a 5)
     (+ a b)
    )
    (f 10)
   )
  )
 )

 (lambda (message args)
  (log "—— Lisa expects referring anassigned variable ——")
  (log "Error message: " message " " (car args))
 )
)
