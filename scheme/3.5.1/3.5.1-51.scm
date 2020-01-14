(include "stream.scm")
(include "streams.scm")

(define (log . args) (for-each display args) (newline))
(define (show x) (log "~> " x) x)

(log "x = (stream-enumerate-range 0 10)")
(define x (stream-map show (stream-enumerate-range 0 10)))

; Prints only zero as 1..10 are delayed:
; x = (stream-enumerate-range 0 10)
; ~> 0

(log "\n(stream-ref x 5) = ...")
(define x5 (stream-ref x 5))
(log "... = " x5 "\n")

; Prints 1..5 as 0 is printed on build,
; and delayed 6..10 are not accessed:
; (stream-ref x 5) = ...
; ~> 1
; ~> 2
; ~> 3
; ~> 4
; ~> 5
; ... = 5


(log "(stream-ref x 7) = ...")
(define x7 (stream-ref x 7))
(log "... = " x7)

; Prints 6..7 as values 1..5 were memoized:
; (stream-ref x 7) = ...
; ~> 6
; ~> 7
; ... = 7
