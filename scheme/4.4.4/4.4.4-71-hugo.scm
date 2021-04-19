;
; So, Hugo Doom thinks to simplify the implementation
; of «simple-query» and «qproc-or» (SICP names
; the latter as «disjoin»)...
;
(
 (lambda () ;<— immediately invoked function
  (define (hugo-query pattern frame-stream)
   (stream-flatmap
    (lambda (frame)
     (stream-append
      (find-assertions pattern frame)
      (apply-rules pattern frame)
     )
    )
    frame-stream
   )
  )

  (define (hugo-disjoin disjuncts frame-stream)
   (if (null? disjuncts)
    the-empty-stream
    (interleave
     (qeval-disp
      (make-pattern (car disjuncts))
      frame-stream
     )
     (hugo-disjoin (cdr disjuncts) frame-stream)
    )
   )
  )

  (set! simple-query hugo-query)
  (set! qproc-or hugo-disjoin)
 )
)
