(include "../4.4.4/qeval-includes-std.scm")

(define count 0)

(define qeval-includes
 (append qeval-includes-std '("4.4.4-76-count.scm"))
)

(include "../4.4.4/qeval-test-base.scm")

(define (log-count)
 (log "—— matches: " count "\n")
 (set! count 0)
)

(define (test-and)
 (log-query
  (and
   (job ?wizard (computer wizard))
   (supervisor ?worker ?wizard)
   (job ?worker (computer programmer))
  )
 )

 (log-count)
)


(log "\n" "—— select computer wizards :" "\n——")

(log-query
 (job ?wizard (computer wizard))
)

(log-count)
; —— matches: 85

(log "—— select programmers supervized by a computer wizard :" "\n——")
(test-and)
; —— matches: 375


; Reset QEval and install improved version of «and» form:
(qeval-reset-test
 (append qeval-includes-std '(
  "4.4.4-76-count.scm"
  "4.4.4-76-and.scm"
 ))
)

(log "—— the same query with «and» optimized :" "\n——")
(test-and)
; —— matches: 220
