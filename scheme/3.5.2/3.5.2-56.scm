(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")

(define (log . args) (for-each display args) (newline))

(define (merge a b)
 (cond
  ((stream-null? a) b)
  ((stream-null? b) a)

  ((< (stream-car a) (stream-car b))
   (cons-stream (stream-car a) (merge (stream-cdr a) b))
  )

  ((< (stream-car b) (stream-car a))
   (cons-stream (stream-car b) (merge a (stream-cdr b)))
  )

  (else
   (cons-stream (stream-car a) (merge (stream-cdr a) (stream-cdr b)))
  )
 )
)

(log "merge (1 3 6 8) (0 1 2 4 5 7 8) = "
 (stream->list
  (merge
   (list->stream '(1 3 6 8))
   (list->stream '(0 1 2 4 5 7 8))
  )
 )
)

(define S
 (cons-stream
  1
  (merge
   (scale-stream 2 S)
   (merge
    (scale-stream 3 S)
    (scale-stream 5 S)
   )
  )
 )
)

(log "S = " (sub-stream->list 100 S) "...")
