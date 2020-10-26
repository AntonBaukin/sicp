
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

; Creates iteration function having no arguments.
; On each call the function returns whether an item
; of the list in the same order, or empty list.
(define (list-iterator lst)
 (lambda ()
  (if (null? lst) '()
   (let ((result (car lst)))
    (set! lst (cdr lst))
    result
   )
  )
 )
)
