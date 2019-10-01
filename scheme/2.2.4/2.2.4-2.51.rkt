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

(define (below2 painter-bottom painter-top)
  (let (
    (bottom (transform-painter2
      painter-bottom
      (make-vect 0 0)
      (make-vect 1 0)
      (make-vect 0 0.5)
    ))
    (top (transform-painter2
      painter-top
      (make-vect 0 0.5)
      (make-vect 1 0.5)
      (make-vect 0 1)
    ))
   )
    (lambda (frame)
      (top frame)
      (bottom frame)
    )
  )
)

(define (rotate90 painter)
  (transform-painter2 painter
    (make-vect 1 0)
    (make-vect 1 1)
    (make-vect 0 0)
  )
)

(define (rotate270 painter)
  (transform-painter2 painter
    (make-vect 0 1)
    (make-vect 0 0)
    (make-vect 1 1)
  )
)

(define (below3 painter-bottom painter-top)
  (rotate90
   (beside
    (rotate270 painter-bottom)
    (rotate270 painter-top)
   )
  )
)


(paint (below2 einstein wave))
(paint (below3 einstein wave))
