(include "2.5.3-rat-base.scm")
(include "2.5.3-polynomial-rat-cut.0.scm")

; Install task-specific version of cut() for sparse terms:
(set-sparse-polynomial-cut (car (make-sparse-polynomial-cut)))

(define n (P 'x 4 1 3 -1 2 -2 1 2))
(define d (P 'x 3 1 1 -1))

; This reduces to: [-x² + 2] / [-x - 1] with GCD [-x² + x]
(log-make-rat n d)

(define a (P 'x 2 -1 0 2))
(define b (P 'x 1 -1 0 -1))
(define g (P 'x 2 -1 1 1))

; Now we check that it's correct:
(log-poly-mul a g)
(log-poly-mul b g)
