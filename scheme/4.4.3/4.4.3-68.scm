(include "../4.4.4/qeval-test-core.scm")

; Instead of append, we apply cut rule,
; just to make it slightly different.
;
(add-rule (cut (?v) () ?v))

(add-rule (cut (?v . ?w) (?v . ?y) ?z)
 (cut ?w ?y ?z)
)

(test-query
 (cut (1 2 3) ?x ?v)
 (cut (1 2 3) (1 2) 3)
)

;
; It's not possible to create symmetric reverse rule
; with not limited evaluator (task 4.67).
;
(add-rule (reverse-right () ()))

(add-rule (reverse-right (?u . ?v) ?z)
 (and
  ; At this point we deduce ?x placing it in the same
  ; position of an unknown argument. If we swap ?v and ?x,
  ; the evaluator hungs as it's not able to split (?u . ?v).
  ; Note that in cut rule it's able to split unknown value
  ; into (?v . ?y) alongside with known (?v . ?w).
  (reverse-right ?v ?x)
  (cut ?z ?x ?u)
 )
)

(add-rule (reverse-left () ()))

(add-rule (reverse-left ?z (?u . ?v))
 (and
  (reverse-left ?x ?v)
  (cut ?z ?x ?u)
 )
)

(log "(reverse-right (1 2 3) ?x) ——> ")
(test-and-log
 (reverse-right (1 2 3) ?x)
 (reverse-right (1 2 3) (3 2 1))
)

(log "\n" "(reverse-left ?x (1 2 3)) ——> ")
(test-and-log
 (reverse-left ?x (1 2 3))
 (reverse-left (3 2 1) (1 2 3))
)
