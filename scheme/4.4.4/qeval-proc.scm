;
; Procedures of QEval: and, or, not, lisp-value.
; Depends on: «stream.scm», «defs.scm», and «utilities.scm».
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
 (if (null? disjuncts)
  the-empty-stream
  (interleave-delayed
   (qeval-disp
    (make-pattern (car disjuncts))
    frame-stream
   )
   (delay
    (qproc-or (cdr disjuncts) frame-stream)
   )
  )
 )
)

(define (qproc-not operands frame-stream)
 (stream-flatmap
  (lambda (frame)
   (define single-frame (singleton-stream frame))
   (define negated-frames
    (qeval-disp
     (make-pattern (car operands))
     single-frame
    )
   )

   (if (stream-null? negated-frames)
    (singleton-stream frame)
    the-empty-stream
   )
  )
  frame-stream
 )
)

(define (lisp-value-match? call frame)
 (eval (prepare-for-eval (instantiate call frame)))
)

(define (qproc-lisp-value call frame-stream)
 (stream-flatmap
  (lambda (frame)
   (if (lisp-value-match? call frame)
    (singleton-stream frame)
    the-empty-stream
   )
  )
  frame-stream
 )
)

(define (qproc-always-true ignore frame-stream)
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
     (list 'always-true qproc-always-true)
    )
   )
  )
 )
)
