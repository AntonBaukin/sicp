(define (log . args) (for-each display args) (newline))

; Special form «let» for analyzing dispatcher is implemented
; here: «4.1.7/eval-impl-forms.scm» — see «eval-disp-form-let».

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")

; These tests are from «4.1.6/eval-disp-test-items.scm»:
(assert-eq? 123
 (eval-basic
  (let (
    (a 100)
    (b 20)
    (c 3)
   )
   (+ a b c)
  )
 )
)

(assert-eq? -123
 (eval-basic
  (let (
    (a 100)
    (b 20)
    (c 3)
   )
   (set! a (- a))
   (set! b (- b))
   (set! c (- c))
   (+ a b c)
  )
 )
)

(assert-eq? 111
 (eval-basic
  (let ((a 100))
   (let ((b (/ a 10)))
    (+ a b 1)
   )
  )
 )
)

(assert-eq? 111
 (eval-basic
  (let ((a 100))
   (let ((b (/ a 10)))
    (let ((c (/ a (* b b))))
     (debug log-env "\n" "—— Environments of three nested lets: ")
     (+ a b c)
    )
   )
  )
 )
)
