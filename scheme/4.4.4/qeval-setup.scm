;
; Assigns default implementation of QEval.
;
(define qeval-setup-std
 (
  (lambda () ;<— immediately invoked function
   (set! qeval-procs (append qeval-procs qeval-procs-std))
  )
 )
)
