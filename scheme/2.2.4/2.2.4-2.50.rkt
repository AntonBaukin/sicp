#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 0)))
(require "2.2.4-2.4x.rkt")

(define (transform-painter2 painter origin corner1 corner2)
  (lambda (frame)
    (let* (
      (m (frame-coord-map frame))
      (o (m origin))
      (x (vector-sub (m corner1) o))
      (y (vector-sub (m corner2) o))
     )
      (painter (make-frame o x y))
    )
  )
)


(define (flip-horiz painter)
  (transform-painter2 painter
    (make-vect 0 1)
    (make-vect 1 1)
    (make-vect 0 0)
  )
)

(define (rotate180 painter)
  (transform-painter2 painter
    (make-vect 1 1)
    (make-vect 0 1)
    (make-vect 1 0)
  )
)

(define (rotate270 painter)
  (transform-painter2 painter
    (make-vect 0 1)
    (make-vect 0 0)
    (make-vect 1 1)
  )
)

(paint wave)
(paint (flip-horiz wave))
(paint (rotate180 wave))
(paint (rotate270 wave))
