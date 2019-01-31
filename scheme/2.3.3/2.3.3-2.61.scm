(define (log . args) (for-each display args) (newline))
(include "2.3.3-sorted-set.scm")

(define NumbersSet (make-sorted-set <))

(log "sorted list (1 2 3 4 5) ?= "
  ((set-op-sorted? NumbersSet) '(1 2 3 4 5)
))

(log "sorted list (1 2 5 4 3) ?= "
  ((set-op-sorted? NumbersSet) '(1 2 5 4 3)
))

(log "(1 2 4 5) + 3 = " ((set-op-add NumbersSet) '(1 2 4 5) 3))
(log "(1 2 3 4) + 5 = " ((set-op-add NumbersSet) '(1 2 3 4) 5))
(log "(1 2 3 4 5) + 5 = " ((set-op-add NumbersSet) '(1 2 3 4 5) 5))

(log "set of (1 2 3 4 5) = " ((set-op-make NumbersSet) '(1 2 3 4 5)))
(log "set of (5 4 3 2 1) = " ((set-op-make NumbersSet) '(5 4 3 2 1)))
(log "set of (1 5 3 3 4 4 1 2) = " ((set-op-make NumbersSet) '(1 5 3 3 4 4 1 2)))

(log "1 ∈ (1 2 3 4 5) ?= " ((set-op-has? NumbersSet) '(1 2 3 4 5) 1))
(log "3 ∈ (1 2 3 4 5) ?= " ((set-op-has? NumbersSet) '(1 2 3 4 5) 3))
(log "5 ∈ (1 2 3 4 5) ?= " ((set-op-has? NumbersSet) '(1 2 3 4 5) 5))
(log "0 ∈ (1 2 3 4 5) ?= " ((set-op-has? NumbersSet) '(1 2 3 4 5) 0))
(log "6 ∈ (1 2 3 4 5) ?= " ((set-op-has? NumbersSet) '(1 2 3 4 5) 6))

(log "(1 2 3 4) ∩ (2 4 6 8) = " ((set-op-intersect NumbersSet) '(1 2 3 4) '(2 4 6 8)))
(log "(1 2 3 4) ∪ (2 4 6 8) = " ((set-op-union NumbersSet) '(1 2 3 4) '(2 4 6 8)))
