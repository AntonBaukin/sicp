(include "qeval-test-base.scm")


; —————————————————————————————————————————————————————————
; —— Section One: Test simple queries for the assertions ——
; —————————————————————————————————————————————————————————

; List all known persons with their positions:
(test-query
 (job ?name ?position)
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


; —————————————————————————————————————————————————————————
; —— Section Two: Test composite queries w/o rules       ——
; —————————————————————————————————————————————————————————

; «And» with two related conditions:
(test-query
 (and
  (supervisor ?subordinate ?supervisor)
  (job ?supervisor (computer . ?position))
 )
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
 (or
  (job (Fect Cy D) (computer programmer))
  (job (Fect Cy D) (computer technician))
 )
 (or
  (job (Tweakit Lem E) (computer programmer))
  (job (Tweakit Lem E) (computer technician))
 )
 (or
  (job (Hacker Alyssa P) (computer programmer))
  (job (Hacker Alyssa P) (computer technician))
 )
)

; Single «Not» produces empty results (see SICP §4.4.2):
(test-query
 (not (job ?person (computer programmer)))
; —————————————————————————————————————————————————————————
)

; «Not» as a filter inside «And»:
(test-query
 (and
  (supervisor ?subordinate ?supervisor)
  (not (job ?supervisor (computer . ?position)))
 )
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
 (and (salary (Scrooge Eben) 75000) (lisp-value > 75000 30000))
 (and (salary (Warbucks Oliver) 150000) (lisp-value > 150000 30000))
 (and (salary (Fect Cy D) 35000) (lisp-value > 35000 30000))
 (and (salary (Hacker Alyssa P) 40000) (lisp-value > 40000 30000))
 (and (salary (Bitdiddle Ben) 60000) (lisp-value > 60000 30000))
)


; —————————————————————————————————————————————————————————
; —— Section Two: Rules                                  ——
; —————————————————————————————————————————————————————————

(add-rule (programmer? ?person)
 (or
  (job ?person (computer programmer . ?rest))
  (job ?person (computer wizard))
 )
)

(test-query
 (programmer? ?name)
; —————————————————————————————————————————————————————————
 (programmer? (Doom Hugo))
 (programmer? (Bitdiddle Ben))
 (programmer? (Fect Cy D))
 (programmer? (Hacker Alyssa P))
)

(add-rule (same ?x ?x))

(add-rule (lives-near? ?person-a ?person-b)
 (and
  (address ?person-a (?town . ?address-a))
  (address ?person-b (?town . ?address-b))
  (not (same ?person-a ?person-b))
 )
)

(test-query
 (lives-near? ?person-a ?person-b)
; —————————————————————————————————————————————————————————
 (lives-near? (Aull DeWitt) (Doom Hugo))
 (lives-near? (Aull DeWitt) (Bitdiddle Ben))
 (lives-near? (Doom Hugo) (Aull DeWitt))
 (lives-near? (Doom Hugo) (Bitdiddle Ben))
 (lives-near? (Hacker Alyssa P) (Fect Cy D))
 (lives-near? (Fect Cy D) (Hacker Alyssa P))
 (lives-near? (Bitdiddle Ben) (Aull DeWitt))
 (lives-near? (Bitdiddle Ben) (Doom Hugo))
)
