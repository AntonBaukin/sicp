
; POSIX rand48 implementation with bits 47..16.
; The same as java.util.Random() does.
; Returns value in [0 .. 4294967295].
;
(define (make-random seed)
 (define a 25214903917)
 (define c 11)
 (define m 281474976710656)

 (lambda ()
  (set! seed (modulo (+ c (* seed a)) m))
  (modulo
   (truncate-quotient seed 65536) ; <— shift 16 bits
   4294967296 ; <— and with 32 bits
  )
 )
)

; Takes random returning 32-bit integer and
; creates random that returns value in [a .. b).
(define (make-random-in-range random a b)
 (define m 4294967295)
 (define r (abs (- a b)))
 (define am (* (min a b) m))

 (define (next) (/ (+ am (* r (random))) m))

 (if (and (exact-integer?  a) (exact-integer?  b))
  (lambda () (truncate (next)))
  next
 )
)

(define (make-random-n-bits seed bits)
 (make-random-in-range (make-random seed) 0 (expt 2 bits))
)
