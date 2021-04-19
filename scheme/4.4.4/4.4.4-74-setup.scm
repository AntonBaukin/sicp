;
; Here we replace flatmap that applies interleaved
; delays with simple mapping of filtered stream...
;
(
 (lambda () ;<— immediately invoked function
  (define (simple-stream-flatmap mapper stream)
   (simple-flatten (stream-map mapper stream))
  )

  (define (stream-not-null? s)
   (not (stream-null? s))
  )

  (define (simple-flatten stream)
   (stream-map stream-car (stream-filter stream-not-null? stream))
  )

  (define (simple-not operands frame-stream)
   (simple-stream-flatmap
    (lambda (frame)
     (define single-frame (singleton-stream frame))
     (define negated-frames
      (qeval-disp
       (make-pattern (car operands))
       single-frame
      )
     )

     (if (stream-null? negated-frames)
      (singleton-stream frame)
      the-empty-stream
     )
    )
    frame-stream
   )
  )

  (define (simple-lisp-value call frame-stream)
   (simple-stream-flatmap
    (lambda (frame)
     (if (lisp-value-match? call frame)
      (singleton-stream frame)
      the-empty-stream
     )
    )
    frame-stream
   )
  )

  ; Unlike SICP, our «find-assertions» uses filter with
  ; predicate created by «make-pattern-matcher».
  (define (simple-find-assertions pattern frame)
   (define matcher (make-pattern-matcher pattern frame))

   (simple-stream-flatmap
    (lambda (datum)
     (define matched-frame (matcher datum))

     (if matched-frame
      (singleton-stream matched-frame)
      the-empty-stream
     )
    )
    (adb-fetch pattern frame)
   )
  )

  (set! qproc-not simple-not)
  (set! qproc-lisp-value simple-lisp-value)
  (set! find-assertions simple-find-assertions)
 )
)
