(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "eval-amb.scm")
(include "eval-amb-test-items.scm")

;(eval-basic (debug on))

