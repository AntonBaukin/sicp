;
; This file includes special samples to demonstrate
; how QEval works with nested frames instead of
; heap frame with unique names.
;
(include "../4.4.4/qeval-test-base.scm")
;
; The implementation of nested frames is included in the core
; implementation of QEval with and is turned on with
; the following instruction:
;
(query '(debug not use unique frames))

; Additional tests without logging:
(include "4.4.4-79-tests.scm")
(include "qeval-test-cases.scm")

(add-rule (same-traced ?x ?x)
 (debug frame ":: same :: ")
)

(add-rule (test-john ?name)
 (and
  (debug frame ">> test-john :: ")
  (same-traced ?name John)
  (debug frame "<< test-john :: ")
 )
)

(log "Trace of variable ?abc deduction :: test-john: ")
(test-query
 (test-john ?abc)
; —————————————————————————————————————————————————————————
 (test-john John)
)

; >> test-john :: frame ((name (? . abc) 0)) 1 (frame () 0 ())
; :: same :: frame ((name John) (x John)) 2 (frame ((name John)) 1 ⏎
;    (frame ((abc John)) 0 ()))
; << test-john :: frame ((name John)) 1 (frame ((abc John)) 0 ())

(log "\n" "Trace of variable ?name deduction :: test-john: ")
(test-query
 (test-john ?name)
; —————————————————————————————————————————————————————————
 (test-john John)
)

; >> test-john :: frame ((name (? . name) 0)) 1 (frame () 0 ())
; :: same :: frame ((name John) (x John)) 2 (frame ((name John)) 1 ⏎
;    (frame ((name John)) 0 ()))
; << test-john :: frame ((name John)) 1 (frame ((name John)) 0 ())

(log "\n" "Trace of defined valriable :: test-john: ")
(test-query
 (test-john John)
; —————————————————————————————————————————————————————————
 (test-john John)
)

; >> test-john :: frame ((name John)) 1 (frame () 0 ())
; :: same :: frame ((x John)) 2 (frame ((name John)) 1 (frame () 0 ()))
; << test-john :: frame ((name John)) 1 (frame () 0 ())

(log "\n" "Trace of defined valriable that do not match :: test-john: ")
(test-query (test-john Ben))

; >> test-john :: frame ((name Ben)) 1 (frame () 0 ())
