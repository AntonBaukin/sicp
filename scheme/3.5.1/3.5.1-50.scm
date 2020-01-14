(include "stream.scm")
(include "streams.scm")

(define (log . args) (for-each display args) (newline))

(define (stream-map mapper . streams)
 (if (stream-null? (car streams))
  the-empty-stream
  (cons-stream
   (apply mapper (map stream-car streams))
   (apply stream-map (cons mapper (map stream-cdr streams)))
  )
 )
)

(define s0 (stream-enumerate-stepped-n 1  9 1))
(define s1 (stream-enumerate-stepped-n 10 9 10))
(define s2 (stream-enumerate-stepped-n 100 9 100))

(define sX (stream-map + s0 s1 s2))

(log 
 (stream->list s0) " + "
 (stream->list s1) " + "
 (stream->list s2) " = "
 (stream->list sX)
)
