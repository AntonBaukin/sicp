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

(define (assert-false? x . reporter)
 (if (not x) #t (assert-report reporter))
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

; Invoke body lambda and checks whether it produces an error:
; if so, optional (may be void or null) catch lambda is invoked
; with arguments: (message error-parameters-list).
; Else, it reports assertion error.
(define (assert-error body catch . reporter)
 (define got #f)

 (with-exception-catcher
  (lambda (e)
   (set! got #t)
   (if (and (procedure? catch) (not (eq? void catch)))
    (cond
     ; Exception raised manually with error?
     ((error-exception? e)
      (catch
       (error-exception-message e)
       (error-exception-parameters e)
      )
     )

     ; Exception objects related to type checking?
     ((type-exception? e)
      (catch
       (string-append
        "(Argument "
        (number->string (type-exception-arg-num e))
        ") "
        (if (symbol? (type-exception-type-id e))
         (symbol->string (type-exception-type-id e))
         "UNKNOWN TYPE"
        )
        " expected"
       )
       (type-exception-arguments e)
      )
     )

     ; Some sort of a fallback:
     (else (catch "" e))
    )
   )
  )
  body
 )

 (if got #t (assert-report reporter))
)
