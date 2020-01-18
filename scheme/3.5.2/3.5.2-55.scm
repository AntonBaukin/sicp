(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")

(define (log . args) (for-each display args) (newline))

(define (partial-sums stream)
 (define sum (add-streams stream (cons-stream 0 sum)))
 sum
)

(define integers (integers-stream 1))
(define isums (partial-sums integers))

(log "(1, 1 + 2, 1 + 2 + 3, ...) = "
 (sub-stream->list 10 isums)
)
