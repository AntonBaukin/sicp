(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "eval-lazy.scm")
(include "../4.1.1/eval-test-items.scm")
(include "../4.1.2/eval-disp-test-items.scm")
(include "../4.1.6/eval-disp-test-items.scm")
(include "../4.1.7/eval-analyze-test-items.scm")
(include "eval-lazy-test-items.scm")
