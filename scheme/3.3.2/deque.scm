(include "deque-ops.scm")

(define Deque (make-deque-ops))

; Creates empty deque.
(define deque-make
 (list-ref Deque 0)
)

; Creates deque from the given list.
(define deque-make-from
 (list-ref Deque 1)
)

(define deque-empty?
 (list-ref Deque 2)
)

; Adds item to the tail of the deque and the last to take.
(define deque-append!
 (list-ref Deque 3)
)
