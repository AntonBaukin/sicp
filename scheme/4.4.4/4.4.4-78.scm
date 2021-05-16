(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "4.4.4-78-amb.scm")
(include "4.4.4-78-stream.scm")
(include "4.4.4-78-qeval.scm")
(include "4.4.4-78-routine.scm")
(include "Microshaft.scm")

; Amb evaluator: combinations of two lists (SICP sample).
(assert-equal?
 '((1 a) (1 b) (2 a) (2 b) (3 a) (3 b))
 (eval-amb-results (list (amb 1 2 3) (amb 'a 'b)))
)

; Amb evaluator: procedure «amb-of».
(assert-equal?
 '((1 a) (1 b) (2 a) (2 b))
 (eval-amb-results (list (amb-of '(1 2)) (amb 'a 'b)))
)

(define (integers-stream from)
 (cons-stream from (integers-stream (+ 1 from)))
)

(define integers (integers-stream 1))
(assert-equal? '(1 2 3 4 5) (sub-stream->list 5 integers))

; We can't define a stream inside Amb using global macros «cons-stream»:
(amb-eval-inject 'integers)

; Amb evaluator easily iterates over infinite streams.
(assert-equal?
 '((1 a) (1 b) (2 a) (2 b) (3 a) (3 b) (4 a) (4 b))
 (eval-amb-lim 8 (list (amb-of-stream integers) (amb 'a 'b)))
)

; Populate QEval Amb with Microshaft database.
(qeval-add-statements qeval Microshaft)
