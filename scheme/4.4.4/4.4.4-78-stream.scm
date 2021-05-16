(include "../3.3.2/iterate.scm")
(include "stream.scm")

; Note, that making streams within Amb evaluator
; is not so simple as injecting a function, as
; «cons-stream» is Gambit macros.
;
; We do not implement this fotm in Amb as we use
; streams only for the database (not to make one
; again with simple lists).
;
(amb-eval-inject 'stream-null?)
(amb-eval-inject 'stream-car)
(amb-eval-inject 'stream-cdr)

(eval-basic
 (define (amb-of-stream stream)
  (require (not (stream-null? stream)))
  (amb (stream-car stream) (amb-of-stream (stream-cdr stream)))
 )

 (global amb-of-stream)
)
