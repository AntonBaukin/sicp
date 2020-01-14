; Produces stream of integers [a; b], «b» — included.
(define (stream-enumerate-range a b)
 (if (> a b)
  the-empty-stream
  (cons-stream a (stream-enumerate-range (+ a 1) b))
 )
)

(define (stream-enumerate-stepped-n step n a)
 (if (<= n 0)
  the-empty-stream
  (cons-stream a (stream-enumerate-stepped-n step (- n 1) (+ a step)))
 )
)
