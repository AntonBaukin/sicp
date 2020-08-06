(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "eval-amb.scm")
;(include "eval-amb-test-items.scm")

; Basic evaluator: assignment.
(assert-eq? 10
 (eval-basic
  (define (make-acc n)
   (lambda (v) (set! n (+ n v)))
  )

  (define acc (make-acc 0))
  (acc 2) (acc 3) (acc 5) ;<— set! returns the value
 )
)

;(eval-amb-result (debug on))

