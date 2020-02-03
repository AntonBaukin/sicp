
(define (integral dt Const signal)
 (define integrated
  (cons-stream
   Const
   (add-streams
    (scale-stream dt signal)
    integrated
   )
  )
 )

 integrated
)

; Returns stream of pairs (i . value @i),
; where «i» steps from 0 by any of «ns».
(define (sample-signal signal . ns)
 (define (rms? i ns)
  (= 0 (apply * (map (lambda (n) (remainder i n)) ns)))
 )

 (stream-filter
  (lambda (is) (rms? (car is) ns))
  (stream-map cons (integers-stream 0) signal)
 )
)
