(include "../4.4.4/qeval-includes-std.scm")

(define qeval-includes
 ; This is the module where we implement the not interleaved behaviour:
 (append qeval-includes-std '("4.4.4-72-b-setup.scm"))
)

;
; Test «B» demonstrates implementation without interleaving.
; It prints triples (triple a b c) where a + b = c.
; Compare with the results for test «A».
;
(include "../4.4.4/qeval-test-core.scm")
(include "4.4.4-72-test.scm")

;
; We print triples by «c» diagonal starting from 9 down to 1.
; Not interleaved evaluation causes the results to go as is.
; This may be handly when the proper order is expected.
;
; (triple 9 1 8 9)
; (triple 9 2 7 9)
; (triple 9 3 6 9)
; (triple 9 4 5 9)
; (triple 9 1 7 8)
; (triple 9 2 6 8)
; (triple 9 3 5 8)
; (triple 9 1 6 7)
; (triple 9 2 5 7)
; (triple 9 3 4 7)
; (triple 9 1 5 6)
; (triple 9 2 4 6)
; (triple 9 1 4 5)
; (triple 9 2 3 5)
; (triple 9 1 3 4)
; (triple 9 1 2 3)
;
