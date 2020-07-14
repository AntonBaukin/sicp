
; Here we replace «apply-basic-lazy()» from «eval-impl-lazy.scm».
(define apply-basic-lazy-28
 (
  (lambda () ;<— immediately invoked function
   (define (apply-basic-lazy procedure arguments env exp)
    ;
    ; In task 28 we do not resolve, i.e. force-it
    ; a procedure before calling it:
    ;
    ; (set! procedure (resolve-value procedure))

    (if (compound-procedure? procedure)
     (apply-basic-compound procedure arguments env exp)
     ; We resolve each thunk before invoking a primitive:
     (underlying-apply procedure (map resolve-value arguments))
    )
   )

   (set! apply-basic apply-basic-lazy)
   apply-basic-lazy ;<— resulting function
  )
 )
)
