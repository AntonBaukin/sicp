;
; Creates universal dispatcher of QEval.
; Depends on: «defs.scm», «qeval-proc.scm», and overrides.
;
(define (find-qproc name tail)
 (cond
  ((null? tail) '())
  ((eq? name (caar tail))
   (cadar tail)
  )
  (else
   (find-qproc name (cdr tail))
  )
 )
)

; Redefine this function to protect the evaluator from infinite
; recursion. Argument «case» is one of: 'push, or 'pop.
;
; On push, return #t to break, or raise an exception, log, etc.
; Return #f to continue the evaluation.
;
; On pop normally return the given frames stream (the result).
;
(define (qeval-disp-protect case query frame-stream)
 (cond
  ((eq? 'push case) #f)
  ((eq? 'pop case) frame-stream)
  (else (error "Wrong QEval protection case" case))
 )
)

(define (qeval-disp-impl pattern frame-stream)
 (define query (untag pattern))
 (define qproc (find-qproc (car query) qeval-procs))

 (cond
  ((qeval-disp-protect 'push query frame-stream)
   the-empty-stream
  )

  ((null? qproc)
   (qeval-disp-protect 'pop query
    (simple-query pattern frame-stream)
   )
  )

  (else
   (qeval-disp-protect 'pop query
    (qproc (cdr query) frame-stream)
   )
  )
 )
)

(define set-qeval-disp
 (
  (lambda () ;<— immediately invoked function
   (set! qeval-disp qeval-disp-impl)
  )
 )
)
