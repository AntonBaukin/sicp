(include "2.5.3-rat-base.scm")
(include "2.5.3-polynomial-rat-cut.0.scm")

; Install task-specific version of cut() for sparse terms:
(set-sparse-polynomial-cut (car (make-sparse-polynomial-cut)))

; Auto-drop all intermediate results:
(toggle-drop-on!)

(define P1 (P 'x 2 1 1 -2 0 1))
(define P2 (P 'x 2 11 0 1))
(define P3 (P 'x 1 13 0 5))

(define Q1 (mul P1 P2))
(define Q2 (mul P1 P3))

(log-poly-mul P1 P2)
(log-poly-mul P1 P3)
(log-make-rat Q1 Q2)

(log "\nTrace these polynomials GCD:")
(set! trace-sparse-polynomial-cut #t)
(make-rat Q1 Q2)
