(include "../4.4.4/qeval-test-base.scm")

(add-rule (same ?x ?x))

(add-rule (independent ?person)
 (and
  (job ?person (?dep . ?position))
  (or
   ; This branch defines a top manager:
   (not (supervisor ?person ?anybody))
   (and
    (supervisor ?person ?super)
    (job ?super (?dep-super . ?position-super))
    (not (same ?dep ?dep-super))
   )
  )
 )
)

(test-and-log
 (independent ?person)
; —————————————————————————————————————————————————————————
 (independent (Warbucks Oliver))
 (independent (Scrooge Eben))
 (independent (Bitdiddle Ben))
)
