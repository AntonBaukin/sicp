(include "../3.3.2/assert.scm")
(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "../3.5.3/signal.scm")

(define (log . args) (for-each display args) (newline))

(define (solve dt y0 f)
 (define y (delayed-integral dt y0 (delay dy)))
 (define dy (stream-map f y))
 y
)

(define est (stream-ref (solve 0.001 1 (lambda (y) y)) 1000))

(log "est " est)
(log "e ≈ 2.718281828459045235360287471352662497757247093699959574966")
;>    est 2.716923932235896
; As you can see, 1000' item gives only 2 digits...

; Now we rewrite integral as the task requires...
(define (delayed-integral dt initial-value delayed-signal)
 (cons-stream
  initial-value
  (let ((signal (force delayed-signal)))
   (if (stream-null? signal)
    the-empty-stream
    (delayed-integral
     dt
     (+ initial-value (* dt (stream-car signal)))
     (delay (stream-cdr signal))
    )
   )
  )
 )
)

; We expect exactly the same result:
(assert-equal? est
 (stream-ref (solve 0.001 1 (lambda (y) y)) 1000)
)
