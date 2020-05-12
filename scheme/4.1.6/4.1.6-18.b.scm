(define (log . args) (for-each display args) (newline))

; Here we use let-transformation from task «4.1.6-16.scm».
; It works the same way as «18.a» — expected behaviour.

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

(include "eval-disp.scm")
(include "eval-streams.scm")

; Enable debug mode:
(eval-basic (debug on))

(include "4.1.6-18-solve.scm")

; This sample is from task «3.5.4-77.scm».
(eval-basic
 (debug log "Computation takes a lot! Please, wait...")
 (define est (stream-ref (solve 0.001 1 (lambda (y) y)) 1000))

 (debug log "est " est) ;> est 2.716923932235896
 (debug log "e ≈ 2.718281828459045235360287471352662497757247093699959574966")
)
