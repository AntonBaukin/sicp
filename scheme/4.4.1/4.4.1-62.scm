(include "../4.4.4/qeval-test-core.scm")

(add-rule (last (?x) (?x)))

(add-rule (last (?v . ?z) ?y)
 (last ?z ?y)
)

(log "(last (3) ?x) ——> ")
(test-and-log
 (last (3) ?x)
 (last (3) (3))
)

(log "\n" "(last (1 2 3) ?x) ——> ")
(test-and-log
 (last (1 2 3) ?x)
 (last (1 2 3) (3))
)

(log "\n" "(last (2 ?x) (3)) ——> ")
(test-and-log
 (last (2 ?x) (3))
 (last (2 3) (3))
)

(log "\n" "And now program hangs on (last ?x (3)) ...")
; It's not possible to deduce infinite set of lists ending with (3):
(log-query (last ?x (3)))
