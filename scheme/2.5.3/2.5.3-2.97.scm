(include "2.5.3-rat-base.scm")
(include "2.5.3-polynomial-rat-cut.1.scm")

; Auto-drop all intermediate results:
(toggle-drop-on!)

(define P1 (P 'x 1 1 0 1))
(define P2 (P 'x 3 1 0 -1))
(define P3 (P 'x 1 1))
(define P4 (P 'x 2 1 0 -1))

; The following fractions has no common terms to reduce:
(define RF1 (make-rat P1 P2))
(log "RF1 = " (num->str RF1))

(define RF2 (make-rat P3 P4))
(log "RF2 = " (num->str RF2))

; Initial implementation of cut() does nothing, output is:
; [x⁴ + x³ + x² - 2x - 1] / [x⁵ - x³ - x² + 1]
(log "Without reduction RF1 + RF2 = " (num->str (add RF1 RF2)))


; Note that we have already implemented all the goals
; of task 2.97 in a generic way. This inlcudes extension
; point of cut() function that calls generic GCD and then
; does the division — it's much better than creating two
; distinct functions: for the GCD and the division.
;
(set-sparse-polynomial-cut (car (make-sparse-polynomial-cut-1)))

; And now make-rat() reduces a term and prints:
; [-x³ - 2x² - 3x - 1] / [-x⁴ - x³ + x + 1]
; Note that it might be negated, but we leave this as-is...
(log "With reduction RF1 + RF2 = " (num->str (add RF1 RF2)))
