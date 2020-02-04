
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

(define (noize-signal random amplitude)
 (define a (exact->inexact amplitude))
 (define r (make-random-in-range random (- a) a))
 (produced-stream r)
)

; Creates stream that takes average of two neighbour values.
(define (average-signal zero-value signal)
 (scale-stream
  0.5
  (add-streams
   signal
   (cons-stream zero-value signal)
  )
 )
)
