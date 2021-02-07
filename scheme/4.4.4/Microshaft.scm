;
; Famous organization database from SICP §4.4.1
; that sounds like, you know...
;
; In russian translation Reasoner Louis is named as Hugo Doom.
; I don't know why such a change, but Hugo is pretty harmful!
;
(define Microshaft '(
 (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
 (job (Bitdiddle Ben) (computer wizard))
 (salary (Bitdiddle Ben) 60000)

 (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
 (job (Hacker Alyssa P) (computer programmer))
 (salary (Hacker Alyssa P) 40000)
 (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

 (address (Fect Cy D) (Cambridge (Ames Street) 3))
 (job (Fect Cy D) (computer programmer))
 (salary (Fect Cy D) 35000)
 (supervisor (Fect Cy D) (Bitdiddle Ben))

 (address (Tweakit Lem E) (Boston (Bay State Road) 22))
 (job (Tweakit Lem E) (computer technician))
 (salary (Tweakit Lem E) 25000)
 (supervisor (Tweakit Lem E) (Bitdiddle Ben))

 (address (Doom Hugo) (Slumerville (Pine Tree Road) 80))
 (job (Doom Hugo) (computer programmer trainee))
 (salary (Doom Hugo) 30000)
 (supervisor (Doom Hugo) (Hacker Alyssa P))

 (supervisor (Bitdiddle Ben) (Warbucks Oliver))

 (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
 (job (Warbucks Oliver) (administration big wheel))
 (salary (Warbucks Oliver) 150000)

 (address (Scrooge Eben) (Weston (Shady Lane) 10))
 (job (Scrooge Eben) (accounting chief accountant))
 (salary (Scrooge Eben) 75000)
 (supervisor (Scrooge Eben) (Warbucks Oliver))

 (address (Cratchet Robert) (Allston (N Harvard Street) 16))
 (job (Cratchet Robert) (accounting scrivener))
 (salary (Cratchet Robert) 18000)
 (supervisor (Cratchet Robert) (Scrooge Eben))

 (address (Aull DeWitt) (Slumerville (Onion Square) 5))
 (job (Aull DeWitt) (administration secretary))
 (salary (Aull DeWitt) 25000)
 (supervisor (Aull DeWitt) (Warbucks Oliver))

 (can-do-job (computer wizard) (computer programmer))
 (can-do-job (computer wizard) (computer technician))
 (can-do-job (computer programmer) (computer programmer trainee))
 (can-do-job (administration secretary) (administration big wheel))
))
