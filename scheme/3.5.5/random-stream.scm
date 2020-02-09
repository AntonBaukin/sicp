
; Stream analog of «3.1/random.scm».
; Takes no commands and does not reset.
(define (make-random-stream seed)
 (define a 25214903917)
 (define c 11)
 (define m 281474976710656)

 (define (next n)
  (modulo (+ c (* n a)) m)
 )

 (define result
  (cons-stream
   (next seed)
   (stream-map next result)
  )
 )

 (define (to32bit n)
  (modulo
   (truncate-quotient n 65536) ; <— shift 16 bits
   4294967296 ; <— and with 32 bits
  )
 )

 (stream-map to32bit result)
)

(define (make-random-stream-in-range a b random-stream)
 (define m 4294967295)
 (define r (abs (- a b)))
 (define am (* (min a b) m))

 (define range
  (if (and (exact-integer? a) (exact-integer? b))
   (lambda (n) (log "T " n " (* r n) " (* r n)) (truncate (/ (+ am (* r n)) m)))
   (lambda (n) (/ (+ am (* r n)) m))
  )
 )

 (stream-map range random-stream)
)
