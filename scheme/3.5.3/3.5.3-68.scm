(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "../3.1/accumulator.scm")

(define (log . args) (for-each display args) (newline))

(define integers (integers-stream 1))

(define (interleave a b)
 (if (stream-null? a) b
  (cons-stream
   (stream-car a)
   (interleave b (stream-cdr a))
  )
 )
)

; Hugo's version of streams pairing. Let's see...
(define (pair-streams a b)
 (interleave
  (stream-map (lambda (x) (cons (stream-car a) x)) b)
  ; This call is not delayed, it's invoked ∞ recursively!
  (pair-streams (stream-cdr a) (stream-cdr b))
 )
)

(define is-is (pair-streams integers integers))
(log "It will never end!..")
