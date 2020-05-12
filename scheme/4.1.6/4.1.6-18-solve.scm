
; Here we copy implementation from task «3.5.4-77.scm».
(eval-basic
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

 (define (solve dt y0 f)
  (define y (delayed-integral dt y0 (delay dy)))
  (define dy (stream-map f y))
  y
 )

 (global solve solve)
)
