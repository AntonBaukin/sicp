(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")

(define (log . args) (for-each display args) (newline))

(define s (cons-stream 1 (add-streams s s)))
(log "s = (cons-stream 1 (add-streams s s))")

; Prints powers of two:
(log "s[0:9] = 2^i = " (sub-stream->list 10 s))
