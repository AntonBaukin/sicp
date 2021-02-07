(include "../4.4.4/qeval-test-base.scm")

; Date format is: (Day-of-week Time-hours).
(define Meetings '(
 (meeting accounting (monday 9))
 (meeting administration (tuesday 10))
 (meeting computer (wednesday 15))
 (meeting company (wednesday 16))
 (meeting administration (friday 13))
))

(qeval-add-statements qeval Meetings)


(log "  ————  " "Meetings on Friday" "  ————")
(test-and-log
 (meeting ?dep (friday 13))
; —————————————————————————————————————————————————————————
 (meeting administration (friday 13))
)


(add-rule (meetings-at ?person ?day-and-time)
 (or
  (meeting company ?day-and-time)
  (and
   (job ?person (?dep . ?position))
   (meeting ?dep ?day-and-time)
  )
 )
)

(define (day? day-and-time day)
 (eq? (car day-and-time) day)
)

(log "\n" "  ————  " "Alyssas Meetings on Wednesday" "  ————")
(test-and-log
 (meetings-at (Hacker Alyssa P) (wednesday ?time))
; —————————————————————————————————————————————————————————
 (meetings-at (Hacker Alyssa P) (wednesday 16))
 (meetings-at (Hacker Alyssa P) (wednesday 15))
)
