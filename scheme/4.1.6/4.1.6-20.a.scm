(define (log . args) (for-each display args) (newline))

; We take implementation from «eval-impl-letrec.scm».
; In version «b» of this task we, as requested, use
; «letrec» made as a transformation to «let».

(define eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.2/eval-impl-disp.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.2/eval-impl-forms.scm"
  "../4.1.6/eval-impl-forms.scm"
  "../4.1.6/eval-impl-letrec.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

(assert-error
 (lambda ()
  (eval-basic
   (let ((fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))))
    (fact 10)
   )
  )
 )
 (lambda (message args)
  (log "—— We expect this error for let-form: ")
  (log "Error message: " message " " (car args))
 )
)

(assert-eq? 3628800
 (eval-basic
  ; With «letrec» factorial lambda is evaluated in the body'
  ; environment as the body itself:
  (letrec ((fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))))
   (fact 10)
  )
 )
)
