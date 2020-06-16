(define (log . args) (for-each display args) (newline))

(define eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.7/eval-impl-registry.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.7/eval-impl-analyze.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.7/eval-impl-forms.scm"
  ; Here we implement «unless» special form:
  "../4.2.1/4.2.1-26-unless.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "../4.1.7/eval-disp.scm")

; Enable debug mode:
(eval-basic (debug on))

; We repeat task «4.2.1-25.scm» using
; special form instead of function.
(eval-basic
 (define (factorial n)
  ; With special form this works well:
  (unless (= 1 n) (* n (factorial (- n 1))) 1)
 )

 (debug log "Special form unless works! "
  "Factorial 5 is: " (factorial 5)
 )
)
