;
; Test «B» demonstrates altered implementation of streams
; flatten without delayed recursion. Compare with test «A».
;
(include "../4.4.4/qeval-includes-std.scm")

(define qeval-includes
 ; This is the module where we implement the not interleaved behaviour:
 (append qeval-includes-std '("4.4.4-73-b-setup.scm"))
)

(include "../4.4.4/qeval-test-core.scm")
(include "4.4.4-73-test.scm")


;
; This sample from task 61 shows that without delay
; delayed stream flattening has no effect other than
; initial computation burst.
;
(add-rule (append () ?y ?y))

(add-rule (append (?u . ?v) ?y (?u . ?z))
 (append ?v ?y ?z)
)

(log "Flattening w/o delay works well for finite streams:")
(test-and-log
 (append (a b) (c d) ?z)
 (append (a b) (c d) (a b c d))
)

; The following query iteration works well in sample «A»:
(log "\n" "But for infinite streams it hangs...")
(log-iter 16 (query-iter (triple ?a ?b ?c)))
