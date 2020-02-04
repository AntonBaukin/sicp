(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "../3.1/accumulator.scm")
(include "../3.3.2/assert.scm")
(include "float-str.scm")

(define (log . args) (for-each display args) (newline))

(define signal-data '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
(define signal (list->stream signal-data))

(define (signal->iv signal)
 (stream-map cons (integers-stream 0) signal)
)

(define (iv->str iv)
 (string-append
  "[@" (number->string (car iv))
  "]" (float->str -1 (cdr iv))
 )
)

(define (signal->str signal)
 (define S (make-concatenator " " iv->str))
 (map S (stream->list signal))
 (S)
)

; Here we see the same signal as in SICP task 3.74:
(log "Source signal: " (signal->str (signal->iv signal)))

; Comparing with SICP's definition, our detector has
; swapped arguments: (prev-value next-value).
(define (sign-change-detector a b)
 (/ (- (if (< b 0) -1 +1) (if (< a 0) -1 +1)) 2)
)

(assert-eq?  0 (sign-change-detector +1 +1))
(assert-eq?  0 (sign-change-detector -1 -1))
(assert-eq?  0 (sign-change-detector  0  0))
(assert-eq? +1 (sign-change-detector -2 +3))
(assert-eq? -1 (sign-change-detector +4 -1))

(define (make-zero-crossings prev-value signal)
 (if (stream-null? signal) the-empty-stream
  (cons-stream
   (sign-change-detector prev-value (stream-car signal))
   (make-zero-crossings
    (stream-car signal)
    (stream-cdr signal)
   )
  )
 )
)

; The same crossings signal as in the task:
(define Lisas-crossings (make-zero-crossings 0 signal))
(log "Lisa's crossings: "
 (signal->str (signal->iv Lisas-crossings))
)

(define zero-crossings
 (stream-map
  sign-change-detector
  ; All this task is for the following line. We swapped it
  ; with the following as detector has the arguments swapped.
  (cons-stream 0 signal) ;<— shift signal to be previous
  signal
 )
)

(log "Task's crossings: "
 (signal->str (signal->iv zero-crossings))
)
