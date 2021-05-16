;
; Make instance of QEval with Amb.
;
(define qeval (make-qeval-amb))
(define query (qeval-query qeval))

(include "qeval-test-support.scm")
