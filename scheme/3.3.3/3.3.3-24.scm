(include "3.3.3-23.scm")

; Continue with the previous implementation that already
; supports nested tables. Full showcase for them is placed
; into separate test file «table-test-nested.scm».

; Create two nested tables 'e and 'f:
((table 'insert!) 5 'e 'f 'g)
(assert-eq? 5 ((table 'lookup) 'e 'f 'g))

; Still, we may not address the lookup to the nested
; table as it's not wrapped into our dispatch, but in
; SICP §3.3.3 the same is not possible either...
; (assert-eq? 5 ((((table 'lookup) 'e 'f) 'lookup) 'g))

((table 'insert!) 6 'e 'h)
(assert-eq? 5 ((table 'lookup) 'e 'f 'g))
(assert-eq? 6 ((table 'lookup) 'e 'h))
