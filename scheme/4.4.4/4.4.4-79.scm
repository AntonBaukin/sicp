(include "../4.4.4/qeval-test-base.scm")

;(add-rule (same ?x ?x))
;
;(add-rule (same-boss ?person ?colleague ?position)
; (and
;  (debug frame "enter")
;  (supervisor ?person ?name)
;  (supervisor ?colleague ?name)
;  (job ?colleague ?position)
;  (not (same ?person ?colleague))
;  (debug frame "exit")
; )
;)
;
;(log-query
;  (and
;   (job ?name (computer . ?position))
;   (same-boss ?name ?colleague ?coposition)
;  )
;)

(query '(debug not use unique frames))

(add-rule (boss-ben? ?name)
 (and
  (debug frame "ENTER boss-ben? :: ")
  (supervisor ?name (Bitdiddle Ben))
 )
)

(log-query
  (and
   (job ?person (computer . ?position))
   (boss-ben? ?person)
  )
)
