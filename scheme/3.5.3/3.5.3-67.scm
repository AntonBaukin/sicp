(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "../3.1/accumulator.scm")

(define (log . args) (for-each display args) (newline))

(define integers (integers-stream 1))

; This sample demonstrates diagonal order of integers
; implemented in «streams.scm» using general weighted
; order of SICP task 3.70.
(define integers-and-integers (pair-streams integers integers))


(define sum 2)
(define S (make-concatenator " "
 (lambda (p)
  (string-append
   ; We split weight equality groups with bullet.
   (if (= sum (+ (car p) (cdr p))) "("
    (begin
     (set! sum (+ (car p) (cdr p)))
     "•️ ("
    )
   )

   (number->string (car p)) " "
   (number->string (cdr p)) ")"
  )
 )
))

(map S (sub-stream->list 92 integers-and-integers))
(log (S))
