(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "../4.4.4/qeval.scm")
(include "../4.4.4/qeval-test-support.scm")

; Make instance of QEval.
(define qeval (make-qeval))
(define query (qeval-query qeval))
(define query-map (qeval-query-map qeval))
(define query-iter-impl (qeval-query-iter qeval))

(define (qeval-reset new-includes)
 (set! qeval-includes new-includes)
 (set! qeval (make-qeval))
 (set! query (qeval-query qeval))
 (set! query-map (qeval-query-map qeval))
 (set! query-iter-impl (qeval-query-iter qeval))
)
