(include "../4.4.4/qeval-test-base.scm")


(log "  ————  " "Supervised by Bitdiddle Ben" "  ————")
(test-and-log
 (supervisor ?person (Bitdiddle Ben))
; —————————————————————————————————————————————————————————
 (supervisor (Tweakit Lem E) (Bitdiddle Ben))
 (supervisor (Fect Cy D) (Bitdiddle Ben))
 (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
)


(log "\n" "  ————  " "Accounting personnel" "  ————")
(test-and-log
 (job ?person (accounting . ?position))
; —————————————————————————————————————————————————————————
 (job (Cratchet Robert) (accounting scrivener))
 (job (Scrooge Eben) (accounting chief accountant))
)


(log "\n" "  ————  " "Slumerville residents" "  ————")
(test-and-log
 (address ?person (Slumerville . ?address))
; —————————————————————————————————————————————————————————
 (address (Aull DeWitt) (Slumerville (Onion Square) 5))
 (address (Doom Hugo) (Slumerville (Pine Tree Road) 80))
 (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
)
