
(define qproc-unique-impl
 (
  (lambda () ;<— immediately invoked function
   (define (singleton-stream? s)
    (and
     (not (stream-null? s))
     (stream-null? (stream-cdr s))
    )
   )

   (define (qproc-unique exp frame-stream)
    (define pattern (make-pattern (car exp)))

    (stream-filter
     (lambda (frame)
      (define results (qeval-disp pattern (singleton-stream frame)))

      (if (singleton-stream? results)
       (stream-car results) ;<— filter works also as a mapper
       #f
      )
     )
     frame-stream
    )
   )

   (set! qeval-procs
    (append
     qeval-procs
     (list
      (list 'unique qproc-unique)
     )
    )
   )

   qproc-unique ;<— resulting function
  )
 )
)
