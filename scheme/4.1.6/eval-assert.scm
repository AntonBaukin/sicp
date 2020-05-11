
; Simplified version of «3.3.2/assert.scm» and «3.3.2/assert-utils.scm».
(eval-basic

 (define (assert-report reporter . vals)
  (define single
   (if (and (pair? reporter) (= 1 (length reporter)))
    (car reporter) void
   )
  )

  (cond
   ((string? single)
    (apply error (cons single (assert-format-vals vals)))
   )

   (else
    (apply error
     (cons "Assertion failed!" (assert-format-vals vals))
    )
   )
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


 ; Define global functions:
 (global assert-eq? assert-eq?)
 (global assert-equal? assert-equal?)
 (global assert-true? assert-true?)
 (global assert-false? assert-false?)
)
