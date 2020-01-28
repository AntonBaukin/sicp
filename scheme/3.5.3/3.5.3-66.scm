(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "../3.1/accumulator.scm")
(include "primes.scm")

(define (log . args) (for-each display args) (newline))


(define integers (integers-stream 1))
(define primes (stream-filter prime? integers))

(log "999983 prime ?= " (prime? 999983))
(log "999984 prime ?= " (prime? 999984))
(log "first 100 primes: " (sub-stream->list 100 primes))
(newline)

(define (interleave-two-streams a b)
 (if (stream-null? a) b
  (cons-stream
   (stream-car a)
   (interleave-two-streams b (stream-cdr a))
  )
 )
)

(define (pair-streams a b)
 (cons-stream
  (cons (stream-car a) (stream-car b))
  (interleave-two-streams
   (stream-map
    (lambda (x) (cons (stream-car a) x))
    (stream-cdr b)
   )
   (pair-streams (stream-cdr a) (stream-cdr b))
  )
 )
)

(define letters (stream-of-list (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
(log "letters stream: " (sub-stream->list 30 letters))
(newline)

(define letters-and-integers (pair-streams letters integers))

(define S (make-concatenator " "
 (lambda (p) (string-append (string (car p)) (number->string (cdr p))))
))

(map S (sub-stream->list 200 letters-and-integers))
(log "letters & integers: " (S))

; It prints:  A1 A2 B2 A3 B3 A4 C3 A5 B4 A6 C4 A7 B5 A8 D4 A9 B6 A10 C5 A11...
; We see that each Ai (i > 1) appears on odd index, and general formula for this
; index: (i - 1) * 2 - 1.
;
; Hence, the number of preceeding items for Ai is: (i - 2) * 2.
; Let's check this...
;

(define (find-li-index letter i)
 (stream-find-index letters-and-integers
  (lambda (p) (and (eq? letter (car p)) (eq? i (cdr p))))
 )
)

(define (get-preceeding letter i)
 (- (find-li-index letter i) 1)
)

(newline)
(log "A100 preceeding = (100 - 2) * 2 = " (get-preceeding #\A 100))

; Index of (100 . 100) is so huge that we are not able to find it in a time.
; Let's find out the stepping of pairs Ai .. Ai+1, Bi .. Bi+1,..

(define (distance letter i)
 (- (find-li-index letter (+ i 1)) (find-li-index letter i) 1)
)

(define (log-distance letter is)
 (for-each
  (lambda (i)
   (log (string letter) i " .. " (+ i 1)
    " I = " (find-li-index letter i)
    " D = " (- (find-li-index letter (+ i 1)) (find-li-index letter i) 1)
   )
  )
  is
 )
)

(newline)
(log-distance #\A '(1 2 3))
(log-distance #\B '(2 3 4))
(log-distance #\C '(3 4 5))
(log-distance #\D '(4 5 6))
(log-distance #\E '(5 6 7))
(log-distance #\F '(6 7 8))

; This trace prints:
;
; A1 .. 2 I = 0 D = 0
; A2 .. 3 I = 1 D = 1
; A3 .. 4 I = 3 D = 1
; B2 .. 3 I = 2 D = 1
; B3 .. 4 I = 4 D = 3
; B4 .. 5 I = 8 D = 3
; C3 .. 4 I = 6 D = 3
; C4 .. 5 I = 10 D = 7
; C5 .. 6 I = 18 D = 7
; D4 .. 5 I = 14 D = 7
; D5 .. 6 I = 22 D = 15
; D6 .. 7 I = 38 D = 15
; E5 .. 6 I = 30 D = 15
; E6 .. 7 I = 46 D = 31
; E7 .. 8 I = 78 D = 31
; F6 .. 7 I = 62 D = 31
; F7 .. 8 I = 94 D = 63
; F8 .. 9 I = 158 D = 63

; It's clear that each pair (i . *) steps from (i + 1 . *) in (2^i - 1) items.
; And we are not able to measure it practically, as (A . 100). Let's calc...
;
; First, (i . *) starts at index Ii = 2^i - 2.
; For (i . j) we always have j >= i, so (i . i) is the first item of (i . *).
;
; (100 . 100) is the first item of (100 . *), and the number of preceeding
; items is I100 = 2^100 - 2.
;
; Let Di be distance between (i . j) and (i . j + 1).
; Note that Di of (i . i) — (i . i + 1) is Di-1, then Di = 2^i - 1.
;
; Pair (99 . 99) is the first of (99 . *).
; It has I99 = 2^99 - 2 items before.
; Distance (99 . 99) — (99 . 100) is D98 = 2^98 - 1.
;
; Hence, the number of items before (99 . 100) is:
; I99 + 1 + D98 - 1= 2^99 + 2^98 - 3 = 3 * (2^98 - 1).
;