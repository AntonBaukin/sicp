(test-query
 (and
  (job ?person (computer . ?position))
  (boss-ben? ?person)
 )
; —————————————————————————————————————————————————————————
 (and (job (Tweakit Lem E) (computer technician)) (boss-ben? (Tweakit Lem E)))
 (and (job (Fect Cy D) (computer programmer)) (boss-ben? (Fect Cy D)))
 (and (job (Hacker Alyssa P) (computer programmer)) (boss-ben? (Hacker Alyssa P)))
)

(test-query
 (and
  (job ?name (computer . ?position))
  (boss-ben? ?name)
 )
; —————————————————————————————————————————————————————————
 (and (job (Tweakit Lem E) (computer technician)) (boss-ben? (Tweakit Lem E)))
 (and (job (Fect Cy D) (computer programmer)) (boss-ben? (Fect Cy D)))
 (and (job (Hacker Alyssa P) (computer programmer)) (boss-ben? (Hacker Alyssa P)))
)





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

;(add-rule (lives-near ?person-a ?person-b)
; (and
;  (debug frame "lives-near ENTER :: ")
;  (address ?person-a (?town . ?address-a))
;  (address ?person-b (?town . ?address-b))
;  (not (same ?person-a ?person-b))
;  (debug frame "lives-near ADDRESS :: ")
; )
;)
;
;(log-query
; (lives-near ?person (Hacker Alyssa P))
;; —————————————————————————————————————————————————————————
;; (lives-near (Fect Cy D) (Hacker Alyssa P))
;)

