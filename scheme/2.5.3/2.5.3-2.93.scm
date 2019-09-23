(include "2.5.3-rat-base.scm")

(define n (P 'x 3 1 0 1))
(define d (P 'x 2 1 0 1))

(log-rat n d)
(log-poly-rat-op "+" add n d n d)