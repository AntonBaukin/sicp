(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "../3.5.3/signal.scm")

(define (log . args) (for-each display args) (newline))

(define (solve-2nd dt y0 dy0 a b)
 (define y (delayed-integral dt y0 (delay dy)))
 (define dy (delayed-integral dt dy0 (delay ddy)))
 (define ddy (add-streams (scale-stream a dy) (scale-stream b y)))
 y
)

; Let's solve equation: y" + y' - 2y = 0, a = -1, b = 2.
; Exact result is: y = C1 * e ^ -2t + C2 * e ^ t.
; At point t0 = 0 it gives y0 = C1 + C2, y'0 = -2C1 + C2.
; With C1 = C2 = 1, y0 = 2, y'0 = -1.
; y(1) = e + e^-2

(log "y\" + y' - 2y = 0, a = -1, b = 2")
(log "y = C1 * e ^ -2t + C2 * e ^ t")
(log "With C1 = C2 = 1, y0 = 2, y'0 = -1")
(log "y(1) = e + e^-2 ≈ ")
(log "  ≈ 2.853617111695657927254286966325146901164878639609535456435")

(log "est "
 (stream-ref
  (solve-2nd 0.001 2 -1 -1 2)
  1000
 )
)
