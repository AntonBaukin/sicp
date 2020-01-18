(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")

(define (log . args) (for-each display args) (newline))

(define integers (integers-stream 1))

(define factorials
 (cons-stream
  1
  (mul-streams
   factorials
   (stream-cdr integers)
  )
 )
)

(log "factorials[0:9] = " (sub-stream->list 10 factorials))