
(define (try-finally body final)
 (with-exception-catcher
  (lambda (e)
   (final)
   (raise e)
  )

  (lambda ()
   (body)
   (final)
  )
 )
)
