(include "../3.5.1/stream.scm")
(include "../3.5.1/streams.scm")
(include "random-stream.scm")

(define (log . args) (for-each display args) (newline))

(define (make-random-stream-cmd cmd-stream)
 (define a 25214903917)
 (define c 11)
 (define m 281474976710656)

 (define (generate? cmd)
  (eq? 'generate cmd)
 )

 (define (reset? cmd)
  (and (pair? cmd) (eq? 'reset (car cmd)))
 )

 (define (next n)
  (modulo (+ c (* n a)) m)
 )

 (define (take n cmd)
  (cond
   ((generate? cmd) (next n))
   ((reset? cmd) (next (cdr cmd)))
   (else (error "Unknown command" cmd))
  )
 )

 (define result
  (cons-stream
   (if (reset? (stream-car cmd-stream))
    (take +nan.0 (stream-car cmd-stream))
    (error "First command must be the reset")
   )
   (stream-map take result (stream-cdr cmd-stream))
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

; Short codes for the commands:
(define commands '(1 G G G 1 G G 2 G G 1 G G 5 G G))

(define (cmd c)
 (if (number? c)
  (cons 'reset (* 1000000007 c))
  'generate
 )
)

(define random-stream
 (make-random-stream-in-range
  0 100
  (make-random-stream-cmd
   (stream-map cmd (list->stream commands))
  )
 )
)

(map log
 (sub-stream->list (length commands)
  (stream-map
   (lambda (n c)
    (string-append
     (number->string n)
     " "
     (if (symbol? c)
      (symbol->string c)
      (string-append
       "(" (symbol->string (car c))
       " " (number->string (cdr c)) ")"
      )
     )
    )
   )
   random-stream
   (stream-map cmd (list->stream commands))
  )
 )
)
