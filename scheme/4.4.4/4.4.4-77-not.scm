
(define qproc-not-mapper-delayed
 (
  (lambda () ;<— immediately invoked function

   (define (qproc-not-delay pattern)
    (lambda (frame)
     (define negated-frames
      (qeval-disp pattern (singleton-stream frame))
     )

     (cond
      ((stream-null? negated-frames) frame)
      ((pattern-resolved? pattern frame) '())
      (else #f)
     )
    )
   )

   (define (qproc-not-mapper-delayed pattern frame)
    (define single-frame (singleton-stream frame))

    (define negated-frames
     (qeval-disp pattern single-frame)
    )

    (cond
     ((stream-null? negated-frames)
      single-frame
     )

     ((pattern-resolved? pattern frame)
      the-empty-stream
     )

     (else
      (singleton-stream
       (frame-add-promise frame (qproc-not-delay pattern))
      )
     )
    )
   )

   (set! qproc-not-mapper qproc-not-mapper-delayed)
  )
 )
)
