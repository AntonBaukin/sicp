(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "../3.4/ts.scm")
(include "../4.1.6/eval-disp.scm")

; Here we run the set of time tests for pure evaluator
; implemented in previous SICP section — §4.1.6.

(log "Time tests for dispatching evaluator:")
(include "4.1.7-24-tests.scm")
;
;   pure recursive Fibonacci [22] took: 18.532 sec
;   co-recursive Fibonacci [10k] took: 4.390 sec
;   reverse of 10k list: 4.314 sec
;
; Time tests for analyzing evaluator:
;   pure recursive Fibonacci [22] took: 10.524 sec
;   co-recursive Fibonacci [10k] took: 3.324 sec
;   reverse of 10k list: 2.989 sec
;
; As we see, for massive function calls analyzing evaluator
; is about 80% faster, and for mixed computations — 25%.
;

