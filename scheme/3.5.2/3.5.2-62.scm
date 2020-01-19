(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "series.scm")

(define (log . args) (for-each display args) (newline))

(define (div-series n d)
 (if (= 0 (stream-car d))
  (error "Divide by power series with zero scalar term!" d)
  (mul-series
   (scale-stream (stream-car d) n)
   (invert-unit-series
    (scale-stream (/ 1 (stream-car d)) d)
   )
  )
 )
)

(define cosine-series
 (cons-stream 1 (neg-series (integrate-series sine-series)))
)

(define sine-series
 (cons-stream 0 (integrate-series cosine-series))
)

(define tangent-series (div-series sine-series cosine-series))

(define pi 3.141592653589793)

; It's clear that this calculation method is not accurate.
; When we add +5 terms, we get only one accuracy digit:
; .9991710667988495 Vs .9999870479375479.
(log "tg (pi / 4) = 1, [10 terms] = "
 (series-sum-at 10 (/ pi 4) tangent-series)
)

(log "tg (pi / 4) = 1, [15 terms] = "
 (series-sum-at 15 (/ pi 4) tangent-series)
)
