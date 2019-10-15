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

; Adds item to the tail of the deque and the last to pop.
(define deque-append!
 (list-ref Deque 3)
)

; Removes the deque's first item and returns it,
; Raises an error on the deque is empty.
(define deque-pop!
 (list-ref Deque 4)
)

; Returns item currently at the head of the deque.
; Raises an error on the deque is empty.
(define deque-first
 (list-ref Deque 5)
)

; Returns item currently at the tail of the deque.
; Raises an error on the deque is empty.
(define deque-last
 (list-ref Deque 6)
)

; Adds item to be the head of the deque and the next to pop.
(define deque-push!
 (list-ref Deque 7)
)

; Invokes visitor callback (second argument) for each item
; of the deque. If it returns value #f, breaks the iteration
; and returns the break item (find behaviour).
(define deque-iterate
 (list-ref Deque 8)
)

(define deque-length
 (list-ref Deque 9)
)

; Removes the item from the tail of the queue
; (the last to pop) and returns it's value.
(define deque-take!
 (list-ref Deque 10)
)
