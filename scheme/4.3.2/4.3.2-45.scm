(define (log . args) (for-each display args) (newline))

(include "../4.3.1/eval-amb.scm")
(include "4.3.2-45-parse.scm")

; Turn on debugging to trace parsing:
; (eval-basic (debug on))

(for-each parse-and-log '(
 (the cat eats)
 (the student with the cat sleeps in the class)
 (the professor lectures to the student with the cat)
 (the professor lectures to the student in the class with the cat)
))

;
; Sample 1:
;
; (sentence (simple-noun-phrase (article the) (noun cat)) (verb eats))

;
; Sample 2:
;
; (sentence
;  (noun-phrase
;   (simple-noun-phrase (article the) (noun student))
;   (prep-phrase
;    (prep with)
;    (simple-noun-phrase (article the) (noun cat))
;   )
;  )
;  (verb-phrase
;   (verb sleeps)
;   (prep-phrase (prep in)
;    (simple-noun-phrase (article the) (noun class))
;   )
;  )
; )

;
; Sample 3:
;
; (sentence
;  (simple-noun-phrase (article the) (noun professor))
;  (verb-phrase
;   (verb-phrase
;    (verb lectures)
;    (prep-phrase
;     (prep to)
;     (simple-noun-phrase (article the) (noun student))
;    )
;   )
;   (prep-phrase
;    (prep with)
;    (simple-noun-phrase (article the) (noun cat)))
;  )
; )

; (sentence
;  (simple-noun-phrase (article the) (noun professor))
;  (verb-phrase
;   (verb lectures)
;   (prep-phrase
;    (prep to)
;    (noun-phrase
;     (simple-noun-phrase (article the) (noun student))
;     (prep-phrase (prep with)
;      (simple-noun-phrase (article the) (noun cat))
;     )
;    )
;   )
;  )
; )

;
; Sample 4 (from the task 45):
;
; (sentence
;  (simple-noun-phrase (article the) (noun professor))
;  (verb-phrase
;   (verb-phrase
;    (verb-phrase
;     (verb lectures)
;     (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student)))
;    )
;    ——> Means: the professor lectures in the class.
;    (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))
;   )
;   ——> Means: the professor has the cat.
;   (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))
;  )
; )

; (sentence
;  (simple-noun-phrase (article the) (noun professor))
;  (verb-phrase
;   (verb-phrase
;    (verb lectures)
;    (prep-phrase
;     (prep to)
;     (simple-noun-phrase (article the) (noun student))
;    )
;   )
;   ——> Means: that lecture takes place in the class where cat lives.
;   (prep-phrase
;    (prep in)
;    (noun-phrase
;     (simple-noun-phrase (article the) (noun class))
;     ——> Means: that the cat lives in the class.
;     (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))
;    )
;   )
;  )
; )

; (sentence
;  (simple-noun-phrase (article the) (noun professor))
;  (verb-phrase
;   (verb-phrase
;    (verb lectures)
;    (prep-phrase
;     (prep to)
;     (noun-phrase
;      (simple-noun-phrase (article the) (noun student))
;      ——> Means: the professor lectures to the student being in the class,
;      ——> and professor may be somewhere outside.
;      (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))
;     )
;    )
;   )
;   ——> Means: also, the professor has the cat (where he is).
;   (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))
;  )
; )

; (sentence
;  (simple-noun-phrase (article the) (noun professor))
;  (verb-phrase
;   (verb lectures)
;   (prep-phrase
;    (prep to)
;    (noun-phrase
;     (noun-phrase
;      (simple-noun-phrase (article the) (noun student))
;      (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))
;     )
;     ——> Means: that professor lectures to the student being in the class,
;     ——>  and to the cat that may be somewhere else.
;     (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))
;    )
;   )
;  )
; )

; (sentence
;  (simple-noun-phrase (article the) (noun professor))
;  (verb-phrase
;   (verb lectures)
;   (prep-phrase
;    (prep to)
;    (noun-phrase
;     (simple-noun-phrase (article the) (noun student))
;     (prep-phrase
;      (prep in)
;      (noun-phrase
;       (simple-noun-phrase (article the) (noun class))
;       ——> Means: that student sits in the class with the cat.
;       (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))
;      )
;     )
;    )
;   )
;  )
; )
