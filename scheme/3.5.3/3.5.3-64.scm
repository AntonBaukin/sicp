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

(define steps 0)

; Supposed to work with endless streams.
(define (stream-limit tolerance stream)
 (define (next a s)
  (define b (stream-car s))
  (if (< (abs (- a b)) tolerance) b
   (begin
    (set! steps (+ steps 1))
    (next b (stream-cdr s))
   )
  )
 )

 (next (stream-car stream) (stream-cdr stream))
)


(define v (stream-limit 0.00001 (sqrt-stream 2.0)))
(log "√2 ≈ " (sqrt 2.0))
(log "@" steps " = " v " more than 5 digits tolerance")
