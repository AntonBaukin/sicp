(include "../4.4.4/qeval-test-core.scm")

(add-rule (append () ?y ?y))

(add-rule (append (?u . ?v) ?y (?u . ?z))
 (append ?v ?y ?z)
)

(log "(append (a b) (c d) ?z) ——> ")
(test-and-log
 (append (a b) (c d) ?z)
 (append (a b) (c d) (a b c d))
)

(log "\n" "(append (a b) ?y (a b c d)) ——> ")
(test-and-log
 (append (a b) ?y (a b c d))
 (append (a b) (c d) (a b c d))
)

(log "\n" "(append ?x ?y (a b c d)) ——> ")
(test-and-log
 (append ?x ?y (a b c d))
; —————————————————————————————————————————————————————————
 (append (a b c d) () (a b c d))
 (append () (a b c d) (a b c d))
 (append (a) (b c d) (a b c d))
 (append (a b) (c d) (a b c d))
 (append (a b c) (d) (a b c d))
)


(add-rule (?x next to ?y in (?x ?y . ?u)))

(add-rule (?x next to ?y in (?v . ?z))
 (?x next to ?y in ?z)
)

(log "\n" "(?x next to ?y in (1 (2 3) 4)) ——> ")
(test-and-log
 (?x next to ?y in (1 (2 3) 4))
; —————————————————————————————————————————————————————————
 ((2 3) next to 4 in (1 (2 3) 4))
 (1 next to (2 3) in (1 (2 3) 4))
)

(log "\n" "(?x next to 1 in (2 1 3 1)) ——> ")
(test-and-log
 (?x next to 1 in (2 1 3 1))
; —————————————————————————————————————————————————————————
 (3 next to 1 in (2 1 3 1))
 (2 next to 1 in (2 1 3 1))
)
