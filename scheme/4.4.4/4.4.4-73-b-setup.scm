;
; Replaces «stream-flatten» with variant not using delay.
;
(
 (lambda () ;<— immediately invoked function
  (define (stream-flatten-sync stream)
   (if (stream-null? stream)
    the-empty-stream
    (interleave
     (stream-car stream)
     (stream-flatten-sync (stream-cdr stream))
    )
   )
  )

  (set! stream-flatten stream-flatten-sync)
 )
)
