(define (log . args) (for-each display args) (newline))

; Here we use default implementation or let-form from §4.1.6,
; see «4.1.6/eval-impl-forms.scm». It works fine, as in task
; «3.5.4-77.scm», but takes a longer time.

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
