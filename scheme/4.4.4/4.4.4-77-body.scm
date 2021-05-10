
(define simple-query-filtered
 (
  (lambda () ;<— immediately invoked function
   (define simple-query-impl simple-query)

   (define (simple-query-filtered pattern frame-stream)
    (stream-filter
     frame-resolve-filter
     (simple-query-impl pattern frame-stream)
    )
   )

   (set! simple-query simple-query-filtered)
  )
 )
)
