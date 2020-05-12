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
  "../4.1.6/4.1.6-18-impl.scm"
  "../4.1.2/eval-impl-forms.scm"
  "../4.1.6/eval-impl-forms.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")
(include "eval-streams.scm")

; Enable debug mode:
(eval-basic (debug on))

(assert-error
 (lambda ()
  (eval-basic
   (
    (lambda (a b)
     (define u (+ a b))
     ; Here we refer previously define variable «u»:
     (define v (* a b u))
     (+ u v)
    )
    2 3
   )
  )
 )

 (lambda (message args)
  (log "—— Expected referring anassigned variable:")
  (log "Error message: " message " " (car args))
 )
)

; In this sample we have the following tranformation:
;
; (let ((u '*unassigned*) (v '*unassigned*))
;  (let (($u (+ a b)) ($v (* a b u)))
;   (set! u $u) (set! v $v) (+ u v)
;  )
; )
;
; In ($v (* a b u)) we see *unassigned* variable «u».
;
; It's clear that transformation in task 4.18 does not
; solve the problem described in §4.1.6.
;
