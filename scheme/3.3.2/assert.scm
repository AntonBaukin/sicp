(include "assert-utils.scm")


(define (assert-eq? a b . reporter)
 (if (eq? a b) #t
  (assert-report reporter a b)
 )
)

(define (assert-equal? a b . reporter)
 (if (equal? a b) #t
  (assert-report reporter a b)
 )
)

(define (assert-true? x . reporter)
 (if x #t (assert-report reporter))
)

; Takes test predicate and single argument.
; Allows to print false value instead of plain #f.
; Test predicate may be a composition.
; On success, returns x-value.
(define (assert-test x test . reporter)
 (if ((compose-assert-test test) x) x
  (assert-report reporter (assert-format-test test) x)
 )
)

(define (assert-test-not x test . reporter)
 (if (not ((compose-assert-test test) x)) x
  (assert-report reporter (assert-format-test test) x)
 )
)
