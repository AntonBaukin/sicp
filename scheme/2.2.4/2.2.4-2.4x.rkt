#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 0)))

; Thanks to Bill the Lizard post on 10/09/2011 for the original coordinates
; www.billthelizard.com/2011/10/sicp-249-defining-primitive-painters.html
;
; Here we draw the segments from a sequance of points, not a sequence
; of lines, thus make the difinitions shorter.
;
(define wave-lines '(
  (0.006 . 0.840) (0.155 . 0.591) (0.304 . 0.646)
  (0.403 . 0.646) (0.348 . 0.845) (0.403 . 0.999)
  () ;<— end of line

  (0.249 . 0.000) (0.354 . 0.492) (0.298 . 0.591)
  (0.155 . 0.392) (0.006 . 0.635)
  () ;<— end of line

  (0.403 . 0.000) (0.502 . 0.293) (0.602 . 0.000)
  () ;<— end of line

  (0.602 . 0.999) (0.652 . 0.845) (0.602 . 0.646)
  (0.751 . 0.646) (0.999 . 0.343)
  () ;<— end of line

  (0.751 . 0.000) (0.597 . 0.442) (0.999 . 0.144)
))

(define frame-outline-lines '(
 (0 . 0) (0 . 0.99) (0.99 . 0.99) (0.99 . 0)
))

(define x-cross-lines '(
 (0 . 0) (1 . 1) () (0 . 1) (1 . 0)
))

(define rhombus-lines '(
 (0 . 0.5) (0.495 . 0.99) (0.99 . 0.495) (0.5 . 0)
))

; Converts broken line defined by a sequence of points
; to the sequence os segments.
(define (lines->segments lines)
  (define (start-segments res list)
    (iter res (car list) (cadr list) (cddr list))
  )

  (define (iter res a b tail)
    (if (null? tail)
      (append res (list (cons a b)))
      (if (null? b)
        (start-segments res tail)
        (iter
          (append res (list (cons a b)))
          b
          (car tail)
          (cdr tail)
        )
      )
    )
  )

  (start-segments '() lines)
)

(define (lines->painter lines)
  (segments->painter (lines->segments lines))
)

; Makes the broken line loop by connecting the last point
; with the first one.
(define (loop-lines->painter lines)
  (lines->painter (append lines (list (car lines))))
)

(define frame-outline (loop-lines->painter frame-outline-lines))
(define rhombus (loop-lines->painter rhombus-lines))
(define x-cross (lines->painter x-cross-lines))
(define wave (lines->painter wave-lines))

(provide frame-outline rhombus x-cross wave)
