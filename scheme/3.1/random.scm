
; POSIX rand48 implementation with bits 47..16.
; The same as java.util.Random() does.
; Returns value in [0 .. 4294967295].
;
; Random takes optional argument:
;  – if it's symbol 'reset, does reset to initial seed;
;  – else it's a new integer value of seed.
;
(define (make-random seed)
 (define n seed)
 (define a 25214903917)
 (define c 11)
 (define m 281474976710656)

 (define (random . cmd)
  (if (null? cmd) void
   (if (eq? 'reset (car cmd))
    (set! n seed)
    (set! n (car seed))
   )
  )

  (set! n (modulo (+ c (* n a)) m))
  (modulo
   (truncate-quotient n 65536) ; <— shift 16 bits
   4294967296 ; <— and with 32 bits
  )
 )

 random
)

; Takes random returning 32-bit integer and
; creates random that returns value in [a .. b).
(define (make-random-in-range random a b)
 (define m 4294967295)
 (define r (abs (- a b)))
 (define am (* (min a b) m))

 (define (next) (/ (+ am (* r (random))) m))

 (if (and (exact-integer? a) (exact-integer? b))
  (lambda () (truncate (next)))
  next
 )
)

; Wrapper for range random. N is positive integer,
; P is ± percent (0 .. 100).
(define (make-random-in-percents random n p)
 (make-random-in-range
  random
  (exact (truncate (- n (* n p 0.01))))
  (+ 1 (exact (truncate (+ n (* n p 0.01)))))
 )
)

(define (make-random-n-bits seed bits)
 (make-random-in-range (make-random seed) 0 (expt 2 bits))
)
