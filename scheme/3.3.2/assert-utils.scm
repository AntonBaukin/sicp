
; Invokes assertion error reporter for the given
; arguments. Note that reporter may be not a plain
; function, but a list of (function ...args).
(define (assert-report reporter . vals)
 (cond
  ((null? reporter)
   (apply
    error
    (cons
     "Assertion failed!"
     (assert-format-vals vals)
    )
   )
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

; Creates 1-predicate from the given composition.
; If test is a procedure, does nothing and returns it.
; Composition must be a list. Zero item is a predicate.
; Following items are it's arguments, that
(define (compose-assert-test test)
 (if (procedure? test) test
  (lambda (x)
   (apply
    (car test) ;<— the predicate
    (compose-assert-args x (cdr test))
   )
  )
 )
)

; Arguments builder for test compositor.
; If there are no arguments, returns list (x).
; Else, replaces each «-» (minus operator) or '-
; (used as a placeholder) with x-value.
(define (compose-assert-args x args)
 (define (ph? item)
  (or (eq? - item) (eq? '- item))
 )

 (define (call-val x val)
  (if (procedure? val) (val x) val)
 )

 (define (replace tail res)
  (if (null? tail) res
   (replace
    (cdr tail)
    (cons
     (if (ph? (car tail)) x (call-val x (car tail)))
     res
    )
   )
  )
 )

 (if (null? args) (list x)
  (reverse (replace args '()))
 )
)

(define (assert-format-vals vals)
 (define (convert-vals res vals)
  (if (null? vals) res
   (let ((v (car vals)))

    ; TODO: print names of functions used for assertions
    (set! v
     (cond
      ((eq? - v) '-)
      ((eq? < v) '<)
      ((eq? > v) '>)
      ((eq? = v) '=)
      (else v)
     )
    )

    (convert-vals (cons v res) (cdr vals))
   )
  )
 )

 (reverse (convert-vals '() vals))
)

(define (assert-format-test test)
 (if (pair? test) (assert-format-vals test) test)
)
