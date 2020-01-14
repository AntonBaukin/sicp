(include "stream.scm")
(include "streams.scm")

(define (log . args) (for-each display args) (newline))

(define sum 0)

(define (accum x)
 (set! sum (+ sum x))
 sum
)

(define seq (stream-map accum (stream-enumerate-range 1 20)))
(log "seq = (stream-map accum (stream-enumerate-range 1 20))")

; Initially sum = 1 as leading item is consumed by stream build:
(log "sum = " sum "\n")

(define y (stream-filter even? seq))

(define (even5? i) (= 0 (remainder i 5)))
(define z (stream-filter even5? seq))

(log "(stream-ref y 7) = " (stream-ref y 7))

; Here sum = 136 as it's the last value computed:
(log "sum = " sum "\n")

(log "z = " (stream->list z))

; Here sum = 210 is the last item of the sequence, and
; is the last item computed. As sequence items are memoized,
; they are not accumulated again, and «sum» is fixed.
(log "sum = " sum "\n")

;(1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)
(log "seq = " (stream->list seq))
