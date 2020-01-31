(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "../3.1/accumulator.scm")

(define (log . args) (for-each display args) (newline))

(define integers (integers-stream 1))


; Pairs (i . j) having i <= j and ordered by i + j ascending.
; This is the same diagonal order as in 67 task, but there
; was full square, not the top triangle i <= j.
(define int-triangle
 (pair-streams-weighted
  integers integers cons
  (lambda (ij)
   (if (> (car ij) (cdr ij))
    ; Trick with infinity allows us to always
    ; skip the streams of upper triangle:
    +inf.0
    (+ (car ij) (cdr ij))
   )
  )
 )
)

(define (ij->s ij)
 (string-append
  "(" (number->string (car ij)) " "
  (number->string (cdr ij)) ")"
 )
)

(define S (make-concatenator " " ij->s))
(map S (sub-stream->list 32 int-triangle))
(log (S))

; (1 1) (1 2) (1 3) (2 2) (1 4) (2 3) (1 5) (2 4) (3 3)
; (1 6) (2 5) (3 4) (1 7) (2 6) (3 5) (4 4) (1 8) ...


(define (w2i+3j+5ij ij)
 (+ (* 2 (car ij)) (* 3 (cdr ij)) (* 5 (car ij) (cdr ij)))
)

(define int-2i+3j+5ij
 (pair-streams-weighted
  integers integers cons
  (lambda (ij)
   (if (> (car ij) (cdr ij))
    +inf.0
    (w2i+3j+5ij ij)
   )
  )
 )
)

(set! S (make-concatenator " "
 (lambda (ij)
  (string-append
   (ij->s ij) "=["
   (number->string (w2i+3j+5ij ij)) "]"
  )
 )
))

(newline)
(map S (sub-stream->list 30 int-2i+3j+5ij))
(log (S))

; This seems to be interleaved stream of task 66:
;
; (1 1)=[10] (1 2)=[18] (1 3)=[26] (2 2)=[30] (1 4)=[34] (1 5)=[42]
; (2 3)=[43] (1 6)=[50] (2 4)=[56] (1 7)=[58] (3 3)=[60] (1 8)=[66]
; (2 5)=[69] (1 9)=[74] (3 4)=[78] (1 10)=[82] (2 6)=[82] ...
