#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 0)))
(require "2.2.4-2.4x.rkt")

(define wave-lines-smile (append wave-lines '(
  () ;<— end of previous line
  (0.42 . 0.78) (0.5 . 0.75) (0.58 . 0.78)
)))

(define wave-smile (lines->painter wave-lines-smile))

(define (right-split n painter)
  (if (= n 0) painter
    (let
      ((smaller (right-split (- n 1) painter)))
      (beside painter (below smaller smaller))
    )
  )
)

(define (up-split n painter)
  (if (= n 0) painter
    (let
      ((smaller (up-split (- n 1) painter)))
      (below painter (beside smaller smaller))
    )
  )
)

(define (corner-split n painter)
  (if (= n 0) painter
    (let* (
      (up (up-split (- n 1) painter))
      (right (right-split (- n 1) painter))
      (top-left (beside up up))
      (bottom-right (below right right))
      (corner (corner-split (- n 1) painter))
     )
      (beside
       (below painter top-left)
       (below bottom-right corner)
      )
    )
  )
)

(define (corner-split2 n painter)
  (if (= n 0) painter
    (let* (
      (up (up-split (- n 1) painter))
      (right (right-split (- n 1) painter))
      (corner (corner-split (- n 1) painter))
     )
      (beside
       (below painter up)
       (below right corner)
      )
    )
  )
)

(define (square-limit n painter)
  (let* (
     (quarter (corner-split n painter))
     (half (beside (flip-horiz quarter) quarter))
    )

    (below (flip-vert half) half)
  )
)

(define (left-split n painter)
  (if (= n 0) painter
    (let
      ((smaller (left-split (- n 1) painter)))
      (beside (below smaller smaller) painter)
    )
  )
)

(define (bottom-split n painter)
  (if (= n 0) painter
    (let
      ((smaller (bottom-split (- n 1) painter)))
      (below (beside smaller smaller) painter)
    )
  )
)

(define (corner-split3 n painter)
  (if (= n 0) painter
    (let* (
      (bottom (bottom-split (- n 1) painter))
      (left (left-split (- n 1) painter))
      (bottom-right (beside bottom bottom))
      (top-left (below left left))
      (corner (corner-split (- n 1) painter))
     )
      (below
       (beside corner bottom-right)
       (beside top-left painter)
      )
    )
  )
)

(define (square-limit2 n painter)
  (let* (
     (quarter (corner-split3 n painter))
     (half (beside (flip-horiz quarter) quarter))
    )

    (below (flip-vert half) half)
  )
)


(paint wave-smile)
(paint (corner-split 2 einstein))
(paint (corner-split2 2 einstein))
(paint (square-limit 2 einstein))
(paint (square-limit2 2 einstein))
