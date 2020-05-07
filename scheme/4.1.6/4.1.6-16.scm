(define (log . args) (for-each display args) (newline))

(define eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.2/eval-impl-disp.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.6/4.1.6-16-impl.scm"
  "../4.1.2/eval-impl-forms.scm"
  "../4.1.6/eval-impl-forms.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

(eval-basic (debug on))


; Test error on referring anassigned variable:
(assert-error
 (lambda ()
  (eval-basic
   (define a '*unassigned*)
   (+ a 1)
  )
 )

 (lambda (message args)
  (log "—— Expected referring anassigned variable:")
  (log "Error message: " message " " (car args))
 )
)

; Test defines to let transformation:
(assert-eq? 39
 (eval-basic
  (define (f a b c)
   (define v (* a b))
   ; With our let-transformation, this works fine:
   (+ a b c v w)
   (define w (* v c))
  )

  (f 2 3 4)
 )
)

; Test definition of a function:
(assert-eq? 24
 (eval-basic
  (define (f i)
   (define (x j) (* j y (z j)))
   (define y 2)
   (x i)
   (define (z j) (+ j 1))
  )
  (f 3)
 )
)
