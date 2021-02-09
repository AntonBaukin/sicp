(include "../4.4.4/qeval-test-core.scm")

(define Names '(
 (son Adam Cain)
 (son Cain Enoch)
 (son Enoch Irad)
 (son Irad Mehujael)
 (son Mehujael Methushael)
 (son Methushael Lamech)
 (wife Lamech Ada)
 (son Ada Jabal)
 (son Ada Jubal)
))

(qeval-add-statements qeval Names)

(add-rule (same ?x ?x))

; Direct or deduced son relationship.
(add-rule (xson ?f ?s)
 (or
  (son ?f ?s)
  (and (son ?w ?s) (wife ?f ?w))
 )
)

(add-rule (grandson ?g ?s)
 (and (xson ?f ?s) (xson ?g ?f))
)

(test-query
 (grandson Adam ?s)
 (grandson Adam Enoch)
)

(add-rule (last (?x) ?x))

(add-rule (last (?v . ?z) ?y)
 (last ?z ?y)
)

(add-rule ((grandson) ?g ?s)
 (grandson ?g ?s)
)

(add-rule ((great . ?rel) ?g ?s)
 (and
  (xson ?g ?x)
  (?rel ?x ?s)
  ; Without this test, the results include:
  ; (great ...  . son), (great ...  . same),
  ; and even (great . grandson).
  (last ?rel grandson)
 )
)

(log "  ————  " "((great grandson) Adam ?x)" "  ————")
(test-and-log
 ((great grandson) Adam ?x)
 ((great grandson) Adam Irad)
)

(log "\n" "  ————  " "((great grandson) ?g ?ggs)" "  ————")
(test-and-log
 ((great grandson) ?g ?ggs)
; —————————————————————————————————————————————————————————
 ((great grandson) Mehujael Jubal)
 ((great grandson) Irad Lamech)
 ((great grandson) Mehujael Jabal)
 ((great grandson) Enoch Methushael)
 ((great grandson) Cain Mehujael)
 ((great grandson) Adam Irad)
)

(log "\n" "  ————  " "(?rel Adam Jabal)" "  ————")
(test-and-log
 (?rel Adam Jabal)
 ((great great great great great grandson) Adam Jabal)
)
