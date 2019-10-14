(include "queue-ops.scm")

(define Queue (make-queue-ops))

; Creates empty queue.
(define queue-make
 (list-ref Queue 0)
)

; Creates queue from the given list.
(define queue-make-from
 (list-ref Queue 1)
)

(define queue-empty?
 (list-ref Queue 2)
)

; Adds item to the tail of the queue and the last to take.
(define queue-append!
 (list-ref Queue 3)
)

; Removes the queue's first item and returns it,
; Raises an error on the queue is empty.
(define queue-take!
 (list-ref Queue 4)
)

; Returns item currently at the head of the queue.
; Raises an error on the queue is empty.
(define queue-first
 (list-ref Queue 5)
)

; Returns item currently at the tail of the queue.
; Raises an error on the queue is empty.
(define queue-last
 (list-ref Queue 6)
)

; Adds item to be the head of the queue and the next to take.
(define queue-push!
 (list-ref Queue 7)
)

; Invokes visitor callback (second argument) for each item
; of the queue. If it returns value #f, breaks the iteration
; and returns the break item (find behaviour).
(define queue-iterate
 (list-ref Queue 8)
)

(define queue-length
 (list-ref Queue 9)
)
