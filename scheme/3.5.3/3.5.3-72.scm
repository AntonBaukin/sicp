(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "../3.1/accumulator.scm")

(define (log . args) (for-each display args) (newline))

(define integers (integers-stream 1))

(define (ij-square ij)
 (+ (square (car ij)) (square (cdr ij)))
)

(define square-weighted-pairs
 (pair-streams-weighted
  integers integers cons
  (lambda (ij)
   (if (> (car ij) (cdr ij))
    +inf.0
    (ij-square ij)
   )
  )
 )
)

(define square-numbers (
 (lambda ()
  (define prev-weight 0)
  (define prev-ijs '(1))

  ; This is a transforming filter.
  (stream-filter
   (lambda (ij)
    (define w (ij-square ij))

    (if (= w prev-weight)
     (begin
      (set! prev-ijs (cons ij prev-ijs))
      #f ; <— we accumulate, but now return false
     )
     (let ((ijs prev-ijs))
      (set! prev-weight w)
      (set! prev-ijs (list ij))
      (if (<= (length ijs) 2) #f
       (reverse ijs)
      )
     )
    )
   )

   square-weighted-pairs
  )
 )
))

(define (sqnum->str ij)
 (string-append
  (number->string (car ij)) "² + "
  (number->string (cdr ij)) "² = "
  (number->string (ij-square ij))
 )
)

(define S (make-concatenator "\n" sqnum->str))
(map S (sub-stream->list 12 square-numbers))
(log (S))
