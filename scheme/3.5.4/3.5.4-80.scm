(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "../3.5.3/signal.scm")
(include "../3.5.3/float-str.scm")

(define (log . args) (for-each display args) (newline))

(define (RLC dt R L C)
 (lambda (vC0 iL0)
  (define vC (delayed-integral dt vC0 (delay dvC)))
  (define iL (delayed-integral dt iL0 (delay diL)))

  (define dvC (scale-stream (/ -1 C) iL))

  (define diL
   (add-streams
    (scale-stream (/ 1 L) vC)
    (scale-stream (/ (- R) L) iL)
   )
  )

  (stream-map cons vC iL)
 )
)

(define rlc (RLC 0.1 1.0 1.0 0.2))

(define (ivi->str ivi)
 (string-append
  "[@" (number->string (car ivi)) "]"
  " v = " (float->str -2 (cadr ivi))
  "\ti = " (float->str -2 (cddr ivi))
 )
)

(define (signal->iv signal)
 (stream-map cons (integers-stream 0) signal)
)

(map log
 (map
  ivi->str
  (sub-stream->list 10
   (signal->iv (rlc 10.0 0.0))
  )
 )
)

; [@0] v = +10.0  i = +0.0
; [@1] v = +10.0  i = +1.0
; [@2] v = +9.5   i = +1.89
; [@3] v = +8.55  i = +2.66
; [@4] v = +7.22  i = +3.24
; [@5] v = +5.59  i = +3.64
; [@6] v = +3.77  i = +3.84
; [@7] v = +1.85  i = +3.83
; [@8] v = -0.6   i = +3.63
; [@9] v = -1.88  i = +3.26
