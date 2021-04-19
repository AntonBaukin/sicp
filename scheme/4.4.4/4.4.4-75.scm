(include "../4.4.4/qeval-includes-std.scm")

(define qeval-includes
 (append qeval-includes-std '("4.4.4-75-unique.scm"))
)

(include "../4.4.4/qeval-test-base.scm")

(test-and-log
 (unique (job ?person (computer wizard)))
 (unique (job (Bitdiddle Ben) (computer wizard)))
)

(test-and-log
 (unique (job ?person (computer programmer)))
)

(add-rule (unique-worker ?person ?position)
 (and
  (job ?person ?position)
  (unique (job ?anyone ?position))
 )
)

(test-and-log
 (unique-worker ?person ?position)
; —————————————————————————————————————————————————————————
 (unique-worker (Aull DeWitt) (administration secretary))
 (unique-worker (Cratchet Robert) (accounting scrivener))
 (unique-worker (Scrooge Eben) (accounting chief accountant))
 (unique-worker (Warbucks Oliver) (administration big wheel))
 (unique-worker (Doom Hugo) (computer programmer trainee))
 (unique-worker (Tweakit Lem E) (computer technician))
 (unique-worker (Bitdiddle Ben) (computer wizard))
)
