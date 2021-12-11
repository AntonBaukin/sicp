(include "../4.4.4/qeval-test-base.scm")
(query '(debug not use unique frames))

(add-rule (append () ?y ?y))

(add-rule (append (?u . ?v) ?y (?u . ?z))
 (append ?v ?y ?z)
)

(test-query
 (append (a) (b) ?z)
; —————————————————————————————————————————————————————————
 (append (a) (b) (a b))
)

(test-query
 (append (a b) (c d) ?z)
; —————————————————————————————————————————————————————————
 (append (a b) (c d) (a b c d))
)

(log-query ;!!! FAILS
 (append ?x ?y (a b c d))
)
