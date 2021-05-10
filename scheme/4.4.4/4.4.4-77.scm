(include "../4.4.4/qeval-includes-std.scm")
(include "../4.4.4/qeval-test-base.scm")

(log "—— Default QEval with leading fetch : ")
(log "——")
(log-query
 (and
  (supervisor ?worker ?head)
  (not (job ?worker (computer programmer)))
 )
)

(log "\n" "—— Default QEval with leading «not» form : ")
(log "—— (no results)")
(log-query
 (and
  (not (job ?worker (computer programmer)))
  (supervisor ?worker ?head)
 )
)

; Reset QEval and install delayed version of «not» form:
(qeval-reset-test
 (append qeval-includes-std '(
  "4.4.4-77-utils.scm"
  "4.4.4-77-body.scm"
  "4.4.4-77-not.scm"
 ))
)

(log "\n" "—— QEval with delayed «not» form : ")
(log "——")
(log-query
 (and
  (not (job ?worker (computer programmer)))
  (supervisor ?worker ?head)
 )
)
