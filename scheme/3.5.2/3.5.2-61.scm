(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "series.scm")

(define (log . args) (for-each display args) (newline))

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

(define exp-series (cons-stream 1 (integrate-series exp-series)))

(define inv-exp-series (invert-unit-series exp-series))

(define e 2.718281828459045)

(log "1/√e ≈ 0.606530659712633423603799534991180453441918135487186955682")

; Look, that we able to use rational arithmetics that resolves
; the problem of intermediate divisions, and we get the same
; answers for sum of first 15 terms e^-0.5 ≡ 1/e^0.5.
(log "e^-0.5 [15 terms] = " (series-sum-at 15 -1/2 exp-series)
 " ≈ " (exact->inexact (series-sum-at 15 -1/2 exp-series))
)

(log "1/e^0.5 [15 terms] = " (series-sum-at 15 1/2 inv-exp-series)
 " ≈ " (exact->inexact (series-sum-at 15 1/2 inv-exp-series))
)
