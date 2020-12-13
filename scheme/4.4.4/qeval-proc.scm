;
; Procedures of QEval: and, or, not, lisp-value.
; Depends on: «stream.scm», and «defs.scm».
;

(define (qproc-and conjuncts frame-stream)
 (if (null? conjuncts)
  frame-stream
  (qproc-and
   (cdr conjuncts)
   (qeval-disp
    (make-pattern (car conjuncts))
    frame-stream
   )
  )
 )
)

(define (qproc-or disjuncts frame-stream)
 frame-stream
)

(define (qproc-not operands frame-stream)
 frame-stream
)

(define (qproc-lisp-value call frame-stream)
 frame-stream
)

; Override this mapping list with your own implementations.
(define set-qeval-procs
 (
  (lambda () ;<— immediately invoked function
   (set! qeval-procs
    (list
     (list 'and qproc-and)
     (list 'or  qproc-or)
     (list 'not qproc-not)
     (list 'lisp-value qproc-lisp-value)
    )
   )
  )
 )
)
