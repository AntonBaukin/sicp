(include "../4.4.4/qeval-test-base.scm")


(log "  ————  " "Supervised by Bitdiddle Ben" "  ————")
(test-and-log
 (and
  (supervisor ?person (Bitdiddle Ben))
  (address ?person ?address)
 )
; —————————————————————————————————————————————————————————
 (and
  (supervisor (Tweakit Lem E) (Bitdiddle Ben))
  (address (Tweakit Lem E) (Boston (Bay State Road) 22))
 )

 (and
  (supervisor (Fect Cy D) (Bitdiddle Ben))
  (address (Fect Cy D) (Cambridge (Ames Street) 3))
 )

 (and
  (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
  (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
 )
)

(log "\n" "  ————  " "Salary Vs Bitdiddle Ben" "  ————")
(test-and-log
 (and
  (salary ?person ?salary)
  (salary (Bitdiddle Ben) ?salary-ben)
  (lisp-value < ?salary ?salary-ben)
 )
; —————————————————————————————————————————————————————————
 (and
  (salary (Aull DeWitt) 25000)
  (salary (Bitdiddle Ben) 60000)
  (lisp-value < 25000 60000)
 )

 (and
  (salary (Cratchet Robert) 18000)
  (salary (Bitdiddle Ben) 60000)
  (lisp-value < 18000 60000)
 )

 (and
  (salary (Doom Hugo) 30000)
  (salary (Bitdiddle Ben) 60000)
  (lisp-value < 30000 60000)
 )

 (and
  (salary (Tweakit Lem E) 25000)
  (salary (Bitdiddle Ben) 60000)
  (lisp-value < 25000 60000)
 )

 (and
  (salary (Fect Cy D) 35000)
  (salary (Bitdiddle Ben) 60000)
  (lisp-value < 35000 60000)
 )

 (and
  (salary (Hacker Alyssa P) 40000)
  (salary (Bitdiddle Ben) 60000)
  (lisp-value < 40000 60000)
 )
)

(define (neq? a b)
 (not (eq? a b))
)

(log "\n" "  ————  " "Not from computer department" "  ————")
(test-and-log-map
 (lambda (frame)
  (list
   "Person:"
   (frame-get frame 'person)
   "Department:"
   (frame-get frame 'dep)
   "Position:"
   (frame-get frame 'position)
   "Supervisor:"
   (frame-get frame 'supervisor)
   "Position:"
   (frame-get frame 'supos)
  )
 )
 (and
  (job ?person (?dep . ?position))
  (lisp-value neq? 'computer (quote ?dep))
  (supervisor ?person ?supervisor)
  (job ?supervisor ?supos)
 )
; —————————————————————————————————————————————————————————
 (
  "Person:" (Aull DeWitt)
  "Department:" administration
  "Position:" (secretary)
  "Supervisor:" (Warbucks Oliver)
  "Position:" (administration big wheel)
 )

 (
  "Person:" (Cratchet Robert)
  "Department:" accounting
  "Position:" (scrivener)
  "Supervisor:" (Scrooge Eben)
  "Position:" (accounting chief accountant)
 )

 (
  "Person:" (Scrooge Eben)
  "Department:" accounting
  "Position:" (chief accountant)
  "Supervisor:" (Warbucks Oliver)
  "Position:" (administration big wheel)
 )
)
