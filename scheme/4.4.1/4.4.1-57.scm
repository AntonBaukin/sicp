(include "../4.4.4/qeval-test-base.scm")

(add-rule (same ?x ?x))

(add-rule (can-replace ?person ?whom)
 (and
  (job ?person ?job-person)
  (job ?whom ?job-whom)
  (not (same ?person ?whom))
  (or
   (same ?job-person ?job-whom)
   (can-do-job ?job-person ?job-whom)
  )
 )
)

(log "  ————  " "Who can replace Fect Cy D" "  ————")
(test-and-log
 (can-replace ?person (Fect Cy D))
; —————————————————————————————————————————————————————————
 (can-replace (Hacker Alyssa P) (Fect Cy D))
 (can-replace (Bitdiddle Ben) (Fect Cy D))
)


(log "\n" "  ————  " "Who can replace whom with greater salary" "  ————")
(test-and-log
 (and
  (can-replace ?person ?whom)
  (salary ?person ?person-salary)
  (salary ?whom ?whom-salary)
  (lisp-value < ?person-salary ?whom-salary)
 )
; —————————————————————————————————————————————————————————
 (and
  (can-replace (Fect Cy D) (Hacker Alyssa P))
  (salary (Fect Cy D) 35000)
  (salary (Hacker Alyssa P) 40000)
  (lisp-value < 35000 40000)
 )

 (and
  (can-replace (Aull DeWitt) (Warbucks Oliver))
  (salary (Aull DeWitt) 25000)
  (salary (Warbucks Oliver) 150000)
  (lisp-value < 25000 150000)
 )
)
