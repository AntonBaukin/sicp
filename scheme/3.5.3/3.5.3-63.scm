(include "../2.3.3/curry.scm")
(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")

(define (log . args) (for-each display args) (newline))


(define sqrt-improves 0)
(define (sqrt-improve x guess)
 (set! sqrt-improves (+ sqrt-improves 1))
 (/ (+ (square guess) x) (* 2 guess))
)

(define (sqrt-stream x)
 (define improve (curry sqrt-improve x))
 (define guesses (cons-stream 1.0 (stream-map improve guesses)))
 guesses
)


(map
 (lambda (p) (log "@" (car p) " = " (cdr p)))
 (sub-stream->list
  10
  (stream-map
   cons
   (integers-stream 0)
   (sqrt-stream 2.0)
  )
 )
)
(log "√2 ≈ " (sqrt 2.0))
; Here sqrt-improves is 10, let's see Hugo's version...
(log "SICP's version calcs " sqrt-improves " improves")


(define (sqrt-stream-hugo x)
 (define improve (curry sqrt-improve x))
 (cons-stream 1.0 (stream-map improve (sqrt-stream-hugo x)))
)

(define (log-hugos n)
 (set! sqrt-improves 0)
 (sub-stream->list n (sqrt-stream-hugo 2.0))
 (log
  (if (< n 10) "  @" " @") n " calcs "
  (if (< sqrt-improves 10) " " "") sqrt-improves " improves"
 )
)


(log "Hugo's version does improves:")
(map log-hugos '(1 2 3 4 5 6 7 8 9 10 11 12))
;  @1 calcs  1 improves
;  @2 calcs  3 improves
;  @3 calcs  6 improves
;  @4 calcs 10 improves
;  @5 calcs 15 improves
;  @6 calcs 21 improves
;  @7 calcs 28 improves
;  @8 calcs 36 improves
;  @9 calcs 45 improves
; @10 calcs 55 improves
; @11 calcs 66 improves
; @12 calcs 78 improves

; So, what's is going on with Hugo's version...
;
; @1 we see 1 improve as when we consume 1.0 item,
; stream map shifts to the next, and it's calced.
;
; @2 when we shift to the next item after 1.0,
; a new instance of the stream is created, and
; we recursively pass it for single stream-cdr,
; thus we do 1 + 2 calcs.
;
; @3 we pass 1 + 2 + 3 calcs.
;
; @i we pass 1 + 2 + .. + i = (1 + i)i / 2
;
; When stream-map shifts stream, it consumes it's head,
; then takes stream-cdr of the same stream instance,
; but Hugo's version updates stream each time, and
; the memizeation is wasted.
;
