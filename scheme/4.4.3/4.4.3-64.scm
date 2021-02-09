(include "../4.4.4/qeval-test-base.scm")

; Original implementation from SICP §4.4.1.
(add-rule (subordinates ?worker ?head)
 (or
  (supervisor ?worker ?head)
  (and
   (supervisor ?worker ?some)
   (subordinates ?some ?head)
  )
 )
)

(log "  ————  " "Rule implemented in book" "  ————")
(test-and-log
 (subordinates ?worker (Bitdiddle Ben))
; —————————————————————————————————————————————————————————
 (subordinates (Tweakit Lem E) (Bitdiddle Ben))
 (subordinates (Doom Hugo) (Bitdiddle Ben))
 (subordinates (Fect Cy D) (Bitdiddle Ben))
 (subordinates (Hacker Alyssa P) (Bitdiddle Ben))
)

(add-rule (xsubordinates ?worker ?head)
 (or
  (supervisor ?worker ?head)
  (and
   ; So, Hugo Doom swaps the lines and goes there into
   ; infinite recursion, as «and» conjuncts are applied
   ; not as a whole, but in-place.
   (xsubordinates ?some ?head)
   (supervisor ?worker ?some)
  )
 )
)

(log "  ————  " "Hugo goes into infinite recursion...")
(log-query (xsubordinates (Bitdiddle Ben) ?who))
