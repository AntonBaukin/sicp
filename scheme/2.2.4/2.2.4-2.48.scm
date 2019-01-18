(include "2.2.4-2.46.scm")

(define (make-segment a b)
 (cons a b)
)

(define (segment-start s)
 (car s)
)

(define (segment-end s)
 (cdr s)
)

; returns segment direction vector V, where a + V = b
(define (segment-vect s)
 (sub-vect (segment-end s) (segment-start s))
)

(define (segment->str s)
 (string-append
  "["
  (vect->str (segment-start s))
  " + "
  (vect->str (segment-vect s))
  " = "
  (vect->str (segment-end s))
  "]"
 )
)

(log "segment " (segment->str (make-segment
  (make-vect 0.0 1.0) (make-vect 2.0 3.0)))
)

