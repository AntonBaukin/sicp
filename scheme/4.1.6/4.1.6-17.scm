(define (log . args) (for-each display args) (newline))

(define eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.2/eval-impl-disp.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.6/4.1.6-16-env.scm"
  "../4.1.6/4.1.6-17-impl.scm"
  "../4.1.2/eval-impl-forms.scm"
  "../4.1.6/eval-impl-forms.scm"
  "../4.1.2/eval-impl-set.scm"
 )
)

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

(include "4.1.6-16-tests.scm")

; Second test, 39, displays the following log:
;
; > Env #3 env-uid-6 let (v w)
; ~> Frame [0 of 1]
;    v .... 6
;    w .... 24
;
; > Env #2 env-uid-5 f (a b c)
; ~> Frame [0 of 1]
;    a .... 2
;    b .... 3
;    c .... 4
;
; > Env #1 env-uid-4 eval-private-scope
; ~> Frame [0 of 1]
;    f .... #<compound-procedure (a b c)>
;
; Here we see additional environment entry created
; for let-form, see «4.1.6/eval-impl-forms.scm».
;
; To avoid this entry, just as task 17 tells, we
; replace let-form with direct define-then-set,
; see «4.1.6-17-impl.scm».
;
; > Env #2 env-uid-4 f (a b c)
; ~> Frame [0 of 1]
;    a .... 2
;    b .... 3
;    c .... 4
;    v .... 6
;    w .... 24
;
; > Env #1 env-uid-3 eval-private-scope
; ~> Frame [0 of 1]
;    f .... #<compound-procedure (a b c)>
;
