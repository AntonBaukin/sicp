(include "../4.4.4/qeval-test-base.scm")
; (include "4.4.4-79-rules.scm")

(query '(debug not use unique frames))
; (include "4.4.4-79-tests.scm")

(add-rule (same ?x ?x))

(add-rule (test ?name)
 (and
  (debug frame "test ENTER :: ")
  (same ?name John)
  (debug frame "test EXIT :: ")
 )
)

(log-query
 (test ?name)
)

