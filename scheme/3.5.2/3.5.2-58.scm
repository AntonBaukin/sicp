(include "../3.5.1/stream.scm")

(define (log . args) (for-each display args) (newline))

; This infinite sequence prints real number, we may also
; extend it to get «.» at the right place.
(define (expand num den radix)
 (cons-stream
  (quotient (* num radix) den)
  (expand (remainder (* num radix) den) den radix)
 )
)

; Fraction 1/7 gives periodic fraction: 0.(142857).
(log "(expand 1 7 10) = " (sub-stream->list 17 (expand 1 7 10)))
(log "1/7 = " (/ 1.0 7))

; This fraction is finite decimal: 0.375, — and the stream
; produces endless zeros: (3 7 5 0 0 0 ...).
(log "(expand 3 8 10) = " (sub-stream->list 10 (expand 3 8 10)))
(log "3/8 = " (/ 3.0 8))
