;
; Compare with «qeval-test.scm» where the same queries
; give the same results with respect to the order.
;

; List all known persons with their positions:
(test-query
 (job ?name ?position)
; —————————————————————————————————————————————————————————
; Same order:
; —————————————————————————————————————————————————————————
 (job (Aull DeWitt) (administration secretary))
 (job (Cratchet Robert) (accounting scrivener))
 (job (Scrooge Eben) (accounting chief accountant))
 (job (Warbucks Oliver) (administration big wheel))
 (job (Doom Hugo) (computer programmer trainee))
 (job (Tweakit Lem E) (computer technician))
 (job (Fect Cy D) (computer programmer))
 (job (Hacker Alyssa P) (computer programmer))
 (job (Bitdiddle Ben) (computer wizard))
)

; «And» with two related conditions:
(test-query
 (and
  (supervisor ?subordinate ?supervisor)
  (job ?supervisor (computer . ?position))
 )
; —————————————————————————————————————————————————————————
; Same order:
; —————————————————————————————————————————————————————————
 (and
  (supervisor (Doom Hugo) (Hacker Alyssa P))
  (job (Hacker Alyssa P) (computer programmer))
 )
 (and
  (supervisor (Tweakit Lem E) (Bitdiddle Ben))
  (job (Bitdiddle Ben) (computer wizard))
 )
 (and
  (supervisor (Fect Cy D) (Bitdiddle Ben))
  (job (Bitdiddle Ben) (computer wizard))
 )
 (and
  (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
  (job (Bitdiddle Ben) (computer wizard))
 )
)

; «Or» with two unrelated conditions:
(test-query
 (or
  (job ?person (computer programmer))
  (job ?person (computer technician))
 )
; —————————————————————————————————————————————————————————
; Order differs (amb Vs interleaved):
; —————————————————————————————————————————————————————————
 (or
  (job (Fect Cy D) (computer programmer))
  (job (Fect Cy D) (computer technician))
 )
 (or
  (job (Hacker Alyssa P) (computer programmer))
  (job (Hacker Alyssa P) (computer technician))
 )
 (or
  (job (Tweakit Lem E) (computer programmer))
  (job (Tweakit Lem E) (computer technician))
 )
)

(test-query
 (and
  (supervisor ?subordinate ?supervisor)
  (not (job ?supervisor (computer . ?position)))
 )
; —————————————————————————————————————————————————————————
; Same order:
; —————————————————————————————————————————————————————————
 (and
  (supervisor (Aull DeWitt) (Warbucks Oliver))
  (not (job (Warbucks Oliver) (computer . ?position)))
 )
 (and
  (supervisor (Cratchet Robert) (Scrooge Eben))
  (not (job (Scrooge Eben) (computer . ?position)))
 )
 (and
  (supervisor (Scrooge Eben) (Warbucks Oliver))
  (not (job (Warbucks Oliver) (computer . ?position)))
 )
 (and
  (supervisor (Bitdiddle Ben) (Warbucks Oliver))
  (not (job (Warbucks Oliver) (computer . ?position)))
 )
)

; «Lisp value» filter inside «And»:
(test-query
 (and
  (salary ?person ?amount)
  (lisp-value > ?amount 30000)
 )
; —————————————————————————————————————————————————————————
; Same order:
; —————————————————————————————————————————————————————————
 (and (salary (Scrooge Eben) 75000) (lisp-value > 75000 30000))
 (and (salary (Warbucks Oliver) 150000) (lisp-value > 150000 30000))
 (and (salary (Fect Cy D) 35000) (lisp-value > 35000 30000))
 (and (salary (Hacker Alyssa P) 40000) (lisp-value > 40000 30000))
 (and (salary (Bitdiddle Ben) 60000) (lisp-value > 60000 30000))
)

; Samples of the rules:
(add-rule (programmer? ?person)
 (or
  (job ?person (computer programmer . ?rest))
  (job ?person (computer wizard))
 )
)

(test-query
 (programmer? ?name)
; —————————————————————————————————————————————————————————
; Order differs:
; —————————————————————————————————————————————————————————
 (programmer? (Doom Hugo))
 (programmer? (Fect Cy D))
 (programmer? (Hacker Alyssa P))
 (programmer? (Bitdiddle Ben))
)

(add-rule (same ?x ?x))

(add-rule (lives-near ?person-a ?person-b)
 (and
  (address ?person-a (?town . ?address-a))
  (address ?person-b (?town . ?address-b))
  (not (same ?person-a ?person-b))
 )
)

(test-query
 (lives-near ?person-a ?person-b)
; —————————————————————————————————————————————————————————
; Order slightly differs:
; —————————————————————————————————————————————————————————
 (lives-near (Aull DeWitt) (Doom Hugo))
 (lives-near (Aull DeWitt) (Bitdiddle Ben))
 (lives-near (Doom Hugo) (Aull DeWitt))
 (lives-near (Doom Hugo) (Bitdiddle Ben))
 (lives-near (Fect Cy D) (Hacker Alyssa P))
 (lives-near (Hacker Alyssa P) (Fect Cy D))
 (lives-near (Bitdiddle Ben) (Aull DeWitt))
 (lives-near (Bitdiddle Ben) (Doom Hugo))
)

(add-rule (programmers-near ?person-a ?person-b)
 (and
  (programmer? ?person-a)
  (programmer? ?person-b)
  (lives-near ?person-a ?person-b)
 )
)

(test-query
 (programmers-near ?person-a ?person-b)
; —————————————————————————————————————————————————————————
 (programmers-near (Doom Hugo) (Bitdiddle Ben))
 (programmers-near (Fect Cy D) (Hacker Alyssa P))
 (programmers-near (Hacker Alyssa P) (Fect Cy D))
 (programmers-near (Bitdiddle Ben) (Doom Hugo))
)
