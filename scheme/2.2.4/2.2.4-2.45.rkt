#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 0)))

(define (split combine-op split-op)
  (define (recurse n painter)
    (if (= n 0) painter
      (let
       ((smaller (recurse (- n 1) painter)))
       (combine-op painter (split-op smaller smaller))
      )
    )
  )
  
  recurse
)

(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split 2 einstein))
(paint (up-split 2 einstein))
