;
; Instead of changing places where interleaved functions
; are invoked, we change the functions themselfs.
;
(
 (lambda () ;<— immediately invoked function
  (define (nointerleave a b)
   (if (stream-null? a) b
    (cons-stream
     (stream-car a)
     ; Do not interleave here:
     (nointerleave (stream-cdr a) b)
    )
   )
  )

  (define (nointerleave-delayed sa sb-delayed)
   (if (stream-null? sa)
    (force sb-delayed)
    (cons-stream
     (stream-car sa)
     (nointerleave-delayed
      (stream-cdr sa)
      sb-delayed
     )
    )
   )
  )

  (set! interleave nointerleave)
  (set! interleave-delayed nointerleave-delayed)
 )
)
