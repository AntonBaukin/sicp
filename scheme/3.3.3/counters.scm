
; Makes increment function. On each call it
; returns current value, «then» increments it.
; Optional argument: initial value (default 0).
(define (make-inc . from)
 (define i (if (= 1 (length from )) (car from) 0))
 (lambda () (let ((r i)) (set! i (+ i 1)) r))
)
