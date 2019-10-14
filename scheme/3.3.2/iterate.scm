
; Invokes visitor callback for each item of the list.
; If visitor returns value #f, breaks the iteration and
; returns the break item (find behaviour).
(define (iterate-list lst visitor)
 (define (next tail)
  (cond
   ((null? tail) void)
   ((eq? #f (visitor (car tail)))
    (car tail)
   )
   (else (next (cdr tail)))
  )
 )

 (next lst)
)
