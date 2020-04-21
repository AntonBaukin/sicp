(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "eval-disp.scm")
;(include "../4.1.1/eval-test-items.scm")
;(include "../4.1.2/eval-disp-test-items.scm")
(include "eval-disp-test-items.scm")
