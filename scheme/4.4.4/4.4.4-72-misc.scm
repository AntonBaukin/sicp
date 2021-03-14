(include "../4.4.4/qeval-test-core.scm")

(add-rule (nl (?x . ?y) ?x)
 (and
  (lisp-value > ?x 0)
  (set ?i - ?x 1)
  (nl ?y ?i)
 )
)

(add-rule (nl (1) 1))

(test-query
 (nl ?x 3)
 (nl (3 2 1) 3)
)

(test-query
 (nl (3 2 1) ?x)
 (nl (3 2 1) 3)
)

(add-rule (car ?x (?x . ?y)))

(add-rule (inc (?x . ?y) ?y)
 (and
  (car ?i ?y)
  (set ?x + ?i 1)
 )
)

(test-query
 (and
  (nl ?a 4)
  (inc ?a ?x)
 )
 (and
  (nl (4 3 2 1) 4)
  (inc (4 3 2 1) (3 2 1))
 )
)

(test-query
 (and
  (nl ?a 4)
  (inc ?x ?a)
 )
 (and
  (nl (4 3 2 1) 4)
  (inc (5 4 3 2 1) (4 3 2 1))
 )
)

(test-query
 (and
  (nl ?a 3)
  (amb ?x ?a)
 )
 (and (nl (3 2 1) 3) (amb 3 (3 2 1)))
 (and (nl (3 2 1) 3) (amb 2 (3 2 1)))
 (and (nl (3 2 1) 3) (amb 1 (3 2 1)))
)

(define (triple? a b c)
 (= (+ (square a) (square b)) (square c))
)

(add-rule (triples-amb ?nl ?a ?b ?c)
 (and
  (amb ?c ?nl)
  (set ?ci - ?c 1)
  (nl ?bl ?ci)
  (amb ?b ?bl)
  (set ?bi - ?b 1)
  (nl ?al ?bi)
  (amb ?a ?al)
  (lisp-value triple? ?a ?b ?c)
 )
)

(add-rule (triple ?n ?a ?b ?c)
 (and
  (nl ?nl ?n)
  (triples-amb ?nl ?a ?b ?c)
 )
)

; Pythagorean triples down from 15:
(test-and-log
 (triple 15 ?a ?b ?c)
 (triple 15 9 12 15)
 (triple 15 5 12 13)
 (triple 15 6 8 10)
 (triple 15 3 4 5)
)
