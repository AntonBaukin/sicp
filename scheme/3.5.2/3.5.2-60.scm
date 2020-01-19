(include "../3.1/accumulator.scm")
(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "series.scm")

(define (log . args) (for-each display args) (newline))

; We will overwrite this for printing coefficients as strings:
(define stream-op* *)
(define stream-op+ +)

(define (mul-series a b)
 (cons-stream
  (stream-op* (stream-car a) (stream-car b))
  (add-streams-with
   stream-op+
   (scale-stream-with stream-op* (stream-car a) (stream-cdr b))
   (scale-stream-with stream-op* (stream-car b) (stream-cdr a))
   ; Here we skip this term of the next power:
   (cons-stream 0 (mul-series (stream-cdr a) (stream-cdr b)))
  )
 )
)

(define (square-series s)
 (mul-series s s)
)

; With «stream-op+» redefined as concatenation of strings,
; here we define sequence of strings as: «a0», «a1», «a2»...
(define (symbol-series letter)
 (add-streams-with
  string-append
  (stream-of letter)
  (stream-map number->string (integers-stream 0))
 )
)

; Just follow the fingers...
(define stream-op* string-append)
(define (stream-op+ . args)
 (define s (make-concatenator " + " (lambda (x) (if (eq? 0 x) "0" x))))
 (for-each s args)
 (s) ;<— a + b + c ...
)

(define series-a (symbol-series "a"))
(define series-b (symbol-series "b"))
(define series-ab (mul-series series-a series-b))

(log "a = " (sub-stream->list 10 series-a))
(log "b = " (sub-stream->list 10 series-b))

(define (log-coeff-ab i)
 (log "a×b [" i "] = " (stream-ref series-ab i))
)

; Here we print the coefficients for resulting terms in a general
; way, as a strings, — as we would write them on paper by hand.
(map log-coeff-ab '(0 1 2 3))

; Return ops back to calculate:
(define stream-op* *)
(define stream-op+ +)

(define cosine-series
 (cons-stream 1 (neg-series (integrate-series sine-series)))
)

(define cosine^2-series (square-series cosine-series))

(define sine-series
 (cons-stream 0 (integrate-series cosine-series))
)

(define sine^2-series (square-series sine-series))

(define sine^2+cosine^2-series
 (add-streams sine^2-series cosine^2-series)
)

(log "sine^2 (0.5) + cosine^2 (0.5) [10 terms] = "
 (series-sum-at 10 0.5 sine^2+cosine^2-series)
)
