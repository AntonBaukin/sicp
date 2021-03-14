;
; With the help of form «set» we are able to build
; up collections, not just deduce them down. This
; allows the rules like «nl» to be used with any
; position of questioned variable, as the tests
; in file «4.4.4-72-misc.scm» show.
;
(add-rule (nl (?x . ?y) ?x)
 (and
  (lisp-value > ?x 0)
  (set ?i - ?x 1)
  (nl ?y ?i)
 )
)

(add-rule (nl (1) 1))

(add-rule (triple ?n ?a ?b ?c)
 (triple-next ?n ?a ?b ?c 1)
)

(add-rule (triple-next ?n ?a ?b ?c ?i)
 (and
  (lisp-value <= ?i ?n)
  (or
   (and
    (set ?i + ?i 1)
    (triple-next ?n ?a ?b ?c ?i)
   )
   (and
    (set ?c ?i)
    (triples-amb ?a ?b ?c)
   )
  )
 )
)

(define (triple? a b c)
 (= (+ a b) c)
)

(add-rule (triples-amb ?a ?b ?c)
 (and
  (set ?ci - ?c 1)
  (nl ?bl ?ci)
  (amb ?b ?bl)
  (set ?bi - ?b 1)
  (nl ?al ?bi)
  (amb ?a ?al)
  (lisp-value triple? ?a ?b ?c)
 )
)

(log-query
 (triple 9 ?a ?b ?c)
)
