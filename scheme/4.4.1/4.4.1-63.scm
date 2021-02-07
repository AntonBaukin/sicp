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


; The main feature here is that we test with
; deduced son relationship, not direct assertion.
(add-rule (grandson ?g ?s)
 (and (xson ?f ?s) (xson ?g ?f))
)

; Direct or deduced son relationship.
(add-rule (xson ?f ?s)
 (or
  (son ?f ?s)
  (and (son ?w ?s) (wife ?f ?w))
 )
)

(test-and-log
 (grandson Cain ?s)
 (grandson Cain Irad)
)

(test-and-log
 (xson Lamech ?s)
 (xson Lamech Jubal)
 (xson Lamech Jabal)
)

(test-and-log
 (grandson Methushael ?s)
 (grandson Methushael Jubal)
 (grandson Methushael Jabal)
)
