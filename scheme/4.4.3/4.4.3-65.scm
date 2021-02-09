(include "../4.4.4/qeval-test-base.scm")

(add-rule (boss ?person)
 (and
  (supervisor ?manager ?person)
  (supervisor ?x ?manager)
 )
)

(log "  ————  " "The query of Fect Cy D" "  ————")
(test-and-log
 (boss ?who)
; —————————————————————————————————————————————————————————
 ; Warbucks Oliver has three workers, two are managers:
 ; Scrooge Eben and Bitdiddle Ben, thus there are four
 ; triples of (?x ?manager (Warbucks Oliver)).
 (boss (Warbucks Oliver))
 (boss (Warbucks Oliver))
 ; Ben has three workers, but only one manager Alyssa.
 (boss (Bitdiddle Ben))
 (boss (Warbucks Oliver))
 (boss (Warbucks Oliver))
)
