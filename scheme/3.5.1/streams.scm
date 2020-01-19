; Produces stream of integers [a; b], «b» — included.
(define (stream-enumerate-range a b)
 (if (> a b)
  the-empty-stream
  (cons-stream a (stream-enumerate-range (+ a 1) b))
 )
)

; Produces n-length ctream of: a, a + step, a + 2*step,..
(define (stream-enumerate-stepped-n step n a)
 (if (<= n 0)
  the-empty-stream
  (cons-stream a (stream-enumerate-stepped-n step (- n 1) (+ a step)))
 )
)

(define (integers-stream from)
 (cons-stream from (integers-stream (+ 1 from)))
)

(define (stream-of value)
 (define s (cons-stream value s))
 s
)

(define (add-streams . streams)
 (apply stream-map (cons + streams))
)

; Used for tests to overwrite meaning of addition.
(define (add-streams-with op . streams)
 (apply stream-map (cons op streams))
)

(define (mul-streams . streams)
 (apply stream-map (cons * streams))
)

(define (mul-streams-with op . streams)
 (apply stream-map (cons op streams))
)

(define (scale-stream number stream)
 (mul-streams (stream-of number) stream)
)

(define (scale-stream-with op number stream)
 (mul-streams-with op (stream-of number) stream)
)

(define (sub-streams a b)
 (apply stream-map (list - a b))
)

(define (div-streams n d)
 (apply stream-map (list / n d))
)
