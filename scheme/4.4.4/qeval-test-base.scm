(include "../4.4.4/qeval-test-core.scm")
(include "../4.4.4/Microshaft.scm")

; Populate QEval with Microshaft database.
(qeval-add-statements qeval Microshaft)

(define (qeval-reset-test new-includes)
 (qeval-reset new-includes)
 (qeval-add-statements qeval Microshaft)
)
