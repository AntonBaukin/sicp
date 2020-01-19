(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "series.scm")

(define (log . args) (for-each display args) (newline))

; Using rational arithmetics with integer coefficients,
; we raise accuracy — see computation of «e».
(log "series of 1/2 powers: " (sub-stream->list 11 (power-series 1/2)))

(define exp-series (cons-stream 1 (integrate-series exp-series)))

(define e 2.718281828459045)

(log "√e ≈ 1.648721270700128146848650787814163571653776100710148011575")

(log "√e [15 terms] = " (series-sum-at 15 1/2 exp-series)
 " ≈ " (exact->inexact (series-sum-at 15 1/2 exp-series))
)

(define cosine-series
 (cons-stream 1 (neg-series (integrate-series sine-series)))
)

(define sine-series
 (cons-stream 0 (integrate-series cosine-series))
)

(define pi 3.141592653589793)

(log "sin (pi / 6) = 0.5, [10 terms] = "
 (series-sum-at 10 (/ pi 6) sine-series)
)

(log "cos (pi / 3) = 0.5, [10 terms] = "
 (series-sum-at 10 (/ pi 3) cosine-series)
)
