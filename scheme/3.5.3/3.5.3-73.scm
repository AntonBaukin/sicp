(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "../3.1/accumulator.scm")
(include "float-str.scm")
(include "signal.scm")

(define (log . args) (for-each display args) (newline))

(define (RC dt R C)
 (lambda (v0 i-signal)
  (add-streams
   (scale-stream R i-signal)
   (scale-stream
    (/ 1.0 C)
    (integral dt v0 i-signal)
   )
  )
 )
)

; Delta time 1ms.
(define dT 0.001)

; One second measure time.
(define T 1.0)

; Number of i-ramp samles.
(define N (exact (truncate (/ T dT))))

; Final current value of 5mA.
(define Imax 0.005)

; Constant current 1mA for second experiment.
(define Iconst 0.001)

; Resistor 1kΩ.
(define R 1000)

; Capacitor 1µF.
(define C 0.000001)

(define (iv->str iv)
 (string-append
  "[@" (number->string (car iv))
  "]" (float->str -8 (cdr iv))
 )
)

; Create current signal ramp raising from 0 to Imax in T.
(define i-ramp (
 (lambda ()
  (define (next i signal)
   (if (= i N)
    (reverse signal)
    (next (+ i 1) (cons (/ (* Imax i) (- N 1)) signal))
   )
  )

  (list->stream (next 0 '()))
 )
))

(define i-const
 (list->stream
  (sub-stream->list N (stream-of Iconst))
 )
)

(define (log-signal signal)
 (define S (make-concatenator " " iv->str))
 (map S (stream->list (sample-signal signal 100 (- N 1))))
 (log (S))
)

(log "i-ramp signal from 0 to "
 (* 1000 Imax) "mA with "
 (stream-length i-ramp) " items:"
)
(log-signal i-ramp)

(define rc (RC dT R C))

(log "\nRC circuit with R = " R
 "Ω and C = " (* 1000000 C) "µF for i-ramp:"
)
(log-signal (rc 0.0 i-ramp))

(log "\nsame RC circuit for constant current "
 (* 1000 Iconst) "mA:"
)
(log-signal (rc 0.0 i-const))
