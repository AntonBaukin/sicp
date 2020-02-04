(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "../3.1/accumulator.scm")
(include "../3.1/random.scm")
(include "float-str.scm")
(include "signal.scm")

(define (log . args) (for-each display args) (newline))

; The same show-case signal as in previous 75 task:
(define signal-data '(1 2 1.5 1 0.15 -0.1 -0.2 -3 -2 -0.1 0.2 3 4))
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

(log "Source signal: " (signal->str (signal->iv signal)))

(define random (make-random 1000003))
(define noize (noize-signal random 0.5))
(define noize-data (sub-stream->list (length signal-data) noize))
(define noize-stream (list->stream noize-data))
(define noizy-signal (add-streams signal noize-stream))

(log "Noize signal:  " (signal->str (signal->iv noize-stream)))
(log "Noizy signal:  " (signal->str (signal->iv noizy-signal)))
(log "Smoothed one:  "
 (signal->str (signal->iv (average-signal 0.0 noizy-signal)))
)

; Comparing with SICP's definition, our detector has
; swapped arguments: (prev-value next-value).
(define (sign-change-detector a b)
 (/ (- (if (< b 0) -1 +1) (if (< a 0) -1 +1)) 2)
)

(define source-crossings
 (stream-map
  sign-change-detector
  (cons-stream 0 signal)
  signal
 )
)

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

(define (smooth-zero-crossings smooth prev-value signal)
 (make-zero-crossings prev-value (smooth prev-value signal))
)

(log "Source crossings: "
 (signal->str (signal->iv source-crossings))
)

(log "Noizy crossings:  "
 (signal->str (signal->iv (make-zero-crossings 0.0 noizy-signal)))
)

(log "Fixed crossings:  "
 (signal->str
  (signal->iv
   (smooth-zero-crossings average-signal 0.0 noizy-signal)
  )
 )
)

; The same result as in the previous task:
;
; Source crossings: +0 +0 +0 +0 +0 -1 +0 +0 +0 +0 +1 +0 +0
; Noizy crossings:  +0 +0 +0 +0 -1 +0 +1 -1 +0 +1 +0 +0 +0
; Fixed crossings:  +0 +0 +0 +0 +0 -1 +0 +0 +0 +0 +1 +0 +0
