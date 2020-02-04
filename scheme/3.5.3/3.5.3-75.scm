(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "../3.1/accumulator.scm")
(include "../3.1/random.scm")
(include "float-str.scm")
(include "signal.scm")

(define (log . args) (for-each display args) (newline))

; This signal is adapted for the following show-case with the noize:
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

(define noizy-crossings
 (stream-map
  sign-change-detector
  (cons-stream 0 noizy-signal)
  noizy-signal
 )
)

(log "Source crossings: "
 (signal->str (signal->iv source-crossings))
)

(log "Noizy crossings:  "
 (signal->str (signal->iv noizy-crossings))
)

; We comment out Hugo's implementation. We do not test it
; on our sample as it still works, apart it's not exactly
; the same as Lisa desired. Let's see...
;
;(define (make-zero-crossings prev-value signal)
; (if (stream-null? signal) the-empty-stream
;  (let ((avpt (/ (+ prev-value (stream-car signal)) 2)))
;   (cons-stream
;    (sign-change-detector prev-value avpt)
;    (make-zero-crossings
;     avpt
;     (stream-cdr signal)
;    )
;   )
;  )
; )
;)

; As it's said in SICP, we add «prev-avpt» argument.
(define (make-zero-crossings prev-avpt prev-value signal)
 (if (stream-null? signal) the-empty-stream
  ; We use exactly previous value, but not previous average:
  (let ((avpt (/ (+ prev-value (stream-car signal)) 2)))
   (cons-stream
    ; Here we compare not previous value ewith current
    ; average, but current one with the previous one:
    (sign-change-detector prev-avpt avpt)
    (make-zero-crossings
     avpt
     (stream-car signal)
     (stream-cdr signal)
    )
   )
  )
 )
)

(log "Fixed crossings:  "
 (signal->str (signal->iv (make-zero-crossings 0.0 0.0 noizy-signal)))
)

; Yes, we created a good sample — smoothing actually works!
;
; Source crossings: +0 +0 +0 +0 +0 -1 +0 +0 +0 +0 +1 +0 +0
; Noizy crossings:  +0 +0 +0 +0 -1 +0 +1 -1 +0 +1 +0 +0 +0
; Fixed crossings:  +0 +0 +0 +0 +0 -1 +0 +0 +0 +0 +1 +0 +0
