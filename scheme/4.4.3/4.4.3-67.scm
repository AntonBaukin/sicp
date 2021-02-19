(include "../4.4.4/qeval-includes-std.scm")
;
; In task 64 Hugo Doom swaps the lines and goes into
; infinite recursion! Let's prevent this...
;
(define qeval-includes
 ; This is the module where we implement the protection:
 (append qeval-includes-std '(
  "../3.3.3/table-list.scm"
  "4.4.3-67-protect.scm"
  "4.4.3-67-inspect.scm"
  "4.4.3-67-setup.scm"
 ))
)

(include "../4.4.4/qeval-test-base.scm")

(add-rule (subordinates ?worker ?head)
 (or
  (supervisor ?worker ?head)
  (and
   (subordinates ?some ?head)
   (supervisor ?worker ?some)
  )
 )
)

;
; The following query prints valid results after the error line:
; QEval ERROR: detected open loop with rule variables some:worker!
;
;(log-query
; (subordinates ?worker (Bitdiddle Ben))
;)

;
; If we trace the query execution, we see theese traces:
;
; (subordinates (? . $1:some) (? . $1:head))
;   (frame ($1:head Bitdiddle Ben) (worker ? . $1:worker))
;   * ()
;
; (subordinates (? . $2:some) (? . $2:head))
;   (frame ($2:head Bitdiddle Ben) ($1:some ? . $2:worker)
;     ($1:head Bitdiddle Ben) (worker ? . $1:worker))
;   * (((some . 1) worker . 2))
;
; (subordinates (? . $3:some) (? . $3:head))
;   (frame ($3:head Bitdiddle Ben) ($2:some ? . $3:worker)
;     ($2:head Bitdiddle Ben) ($1:some ? . $2:worker)
;     ($1:head Bitdiddle Ben) (worker ? . $1:worker))
;   * (((some . 1) worker . 2) ((some . 2) worker . 3))
;
; After each frame goes line marked with «*». It is the list
; of rule variables in form (name . uid) that were not resolved
; into concrete values.
;
; Infinite recursion means that theese variables would never
; be resolved, plus, they form repeating pattern.
;
; Closed loop of dependencies means that references form a cycle
; in the graph. But in this sample this is not true. See, lines:
;
; (subordinates (? . $1:some) (Bitdiddle Ben))
; (subordinates (? . $2:some) (Bitdiddle Ben))
; (subordinates (? . $3:some) (Bitdiddle Ben))
;
; never end. Details on how this open loop is detected are given
; as comments to «make-open-loop-detector» function in file
; «4.4.3-67-inspect.scm».
;

;
; The same method also works with last sample of task 62.
;
(add-rule (last (?x) (?x)))

(add-rule (last (?v . ?z) ?y)
 (last ?z ?y)
)

; QEval ERROR: detected open loop with rule variables pair «z:v»
(log-query (last ?x (3)))
