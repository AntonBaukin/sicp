
; Invokes assertion error reporter for the given
; arguments. Note that reporter may be not a plain
; function, but a list of (function ...args).
(define (assert-report reporter . vals)
 (cond
  ((null? reporter)
   (apply error (cons "Assertion failed!" vals))
  )
  ((procedure? reporter)
   (apply reporter vals)
  )
  (else
   (apply
    (car reporter)
    (append (cdr reporter) vals)
   )
  )
 )
)

(define (assert-eq? a b . reporter)
 (if (eq? a b) #t
  (assert-report reporter a b)
 )
)

(define (assert-true? x . reporter)
 (if x #t (assert-report reporter x))
)

; Takes test predicate and single arguments.
; Allows to print false value instead of plain #f.
(define (assert-test test x . reporter)
 (if (test x) #t (assert-report reporter test x))
)

(define (assert-test-not test x . reporter)
 (if (not (test x)) #t (assert-report reporter test x))
)
