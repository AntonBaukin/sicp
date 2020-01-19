
(define (integrate-series series)
 (div-streams series (integers-stream 1))
)

(define (partial-sums series)
 (define sum (add-streams series (cons-stream 0 sum)))
 sum
)

; Returns powerer of «x» series starting with 1 (power 0).
(define (power-series x)
 (define series (cons-stream 1 powers))
 (define powers (mul-streams (stream-of x) series))
 series
)

; Takes series of coefficients of infinite power series
; and returns partial sum at given point «x». Limits
; the number of terms to «n».
(define (series-sum-at n x series)
 (stream-ref
  (partial-sums
   (mul-streams
    (power-series x)
    series
   )
  )
  n
 )
)

(define zeros (stream-of 0))

(define (neg-series series)
 (sub-streams zeros series)
)

(define (mul-series a b)
 (cons-stream
  (* (stream-car a) (stream-car b))
  (add-streams
   (scale-stream (stream-car a) (stream-cdr b))
   (scale-stream (stream-car b) (stream-cdr a))
   ; Here we skip this term of the next power:
   (cons-stream 0 (mul-series (stream-cdr a) (stream-cdr b)))
  )
 )
)

(define (invert-unit-series series)
 (define inv
  (cons-stream
   1
   (neg-series
    (mul-series
     (stream-cdr series)
     inv
    )
   )
  )
 )

 inv
)
