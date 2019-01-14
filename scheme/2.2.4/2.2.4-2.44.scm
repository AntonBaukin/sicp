#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 0)))

; Paints in clear order: left and right
(paint (beside einstein diagonal-shading))

(define (right-split n painter)
  (if (= n 0) painter
    (let
      ((smaller (right-split (- n 1) painter)))
      (beside painter (below smaller smaller))
    )
  )
)

(paint (right-split 2 einstein))

; Paints in reversed order: diagonal is above! 
(paint (below einstein diagonal-shading))

(define (up-split n painter)
  (if (= n 0) painter
    (let
      ((smaller (up-split (- n 1) painter)))
      (below painter (beside smaller smaller))
    )
  )
)

(paint (up-split 2 einstein))

(define (corner-split n painter)
  (if (= n 0) painter
    (let*
     (
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

(paint (corner-split 2 einstein))

(define (square-limit n painter)
  (let*
    (
     (quarter (corner-split n painter))
     (half (beside (flip-horiz quarter) quarter))
    )

    (below (flip-vert half) half)
  )
)

(paint (square-limit 2 einstein))