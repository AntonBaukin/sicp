(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "../3.1/accumulator.scm")

(define (log . args) (for-each display args) (newline))

(define integers (integers-stream 1))

(define (ij-cube ij)
 (define i (car ij))
 (define j (cdr ij))
 (+ (* i i i) (* j j j))
)

(define cube-weighted-pairs
 (pair-streams-weighted
  integers integers cons
  (lambda (ij)
   (if (> (car ij) (cdr ij))
    +inf.0
    (ij-cube ij)
   )
  )
 )
)

; This implementation is more than required.
; It suites for the following task 3.72.
; It accumulates the pairs to print them instead
; of the resulting Ramanujian numbers.
(define ramanujian-numbers (
 (lambda ()
  (define prev-weight 0)
  (define prev-ijs '(1))

  ; This is a transforming filter.
  (stream-filter
   (lambda (ij)
    (define w (ij-cube ij))

    (if (= w prev-weight)
     (begin
      (set! prev-ijs (cons ij prev-ijs))
      #f ; <— we accumulate, but now return false
     )
     (let ((ijs prev-ijs))
      (set! prev-weight w)
      (set! prev-ijs (list ij))
      (if (= 1 (length ijs)) #f
       (reverse ijs)
      )
     )
    )
   )

   cube-weighted-pairs
  )
 )
))

(define (ramnum->str ij)
 (string-append
  (number->string (car ij)) "³ + "
  (number->string (cdr ij)) "³ = "
  (number->string (ij-cube ij))
 )
)

(define S (make-concatenator "\n" ramnum->str))
(map S (sub-stream->list 12 ramanujian-numbers))
(log (S))
