(define (log . args) (for-each display args) (newline))
(include "2.3.3-sorted-set.scm")

(define NumbersSet (make-sorted-set <))

(log "(1 2 3 4) ∩ (2 4 6 8) = " ((set-op-intersect NumbersSet) '(1 2 3 4) '(2 4 6 8)))
(log "(1 2 3 4) ∪ (2 4 6 8) = " ((set-op-union NumbersSet) '(1 2 3 4) '(2 4 6 8)))
