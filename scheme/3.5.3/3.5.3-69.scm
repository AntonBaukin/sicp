(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "../3.1/accumulator.scm")
(include "../3.3.2/assert.scm")

(define (log . args) (for-each display args) (newline))


(define integers (integers-stream 1))

(define ijk-full (triple-streams integers integers integers))

(define (ijk->s ijk)
 (string-append
  "(" (number->string (car ijk)) " "
  (number->string (cadr ijk)) " "
  (number->string (caddr ijk)) ")="
  (number->string (apply + ijk))
 )
)

(define S (make-concatenator " " ijk->s))
(map S (sub-stream->list 96 ijk-full))
(log "All (i j k) triples: " (S))


(define (check-unique items)
 (define mem '())

 (define (check i tail)
  (if (null? tail)
   (set! mem (cons i mem))
   (if (equal? i (car tail))
    (error "Duplicate triple found!" i)
    (check i (cdr tail))
   )
  )
 )

 (for-each (lambda (i) (check i mem)) items)
)

; Here we check that triples do not repeat:
(check-unique (sub-stream->list 1000 ijk-full))

(define ijk-lower
 (triple-streams-weighted
  integers integers integers
  list
  (lambda (ijk)
   (if
    (and
     (<= (car ijk) (cadr ijk))
     (<= (cadr ijk) (caddr ijk))
    )
    (apply + ijk)
    ; Trick with infinity allows us to always
    ; skip the streams of upper triangle:
    +inf.0
   )
  )
 )
)

(set! S (make-concatenator " " ijk->s))
(map S (sub-stream->list 96 ijk-lower))
(log "\n(i <= j <= k) triples: " (S))

(define pythagorean-triples
 (stream-filter
  (lambda (ijk)
   (=
    (+ (square (car ijk)) (square (cadr ijk)))
    (square (caddr ijk))
   )
  )
  ijk-lower
 )
)

(set! S (make-concatenator " " ijk->s))
(map S (sub-stream->list 10 pythagorean-triples))

(log "\nPythagorean triples: " (S))
;
; :> (3 4 5)=12 (6 8 10)=24 (5 12 13)=30 (9 12 15)=36 (8 15 17)=40
; (12 16 20)=48 (7 24 25)=56 (10 24 26)=60 (15 20 25)=60 (20 21 29)=70
;
