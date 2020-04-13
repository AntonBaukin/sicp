(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "eval-basic.scm")
(include "eval-test-items.scm")
