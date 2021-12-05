(add-rule (same ?x ?x))

(add-rule (boss-ben? ?name)
 (and
  ; (debug frame "ENTER boss-ben? :: ")
  (supervisor ?name (Bitdiddle Ben))
 )
)
