;
; Creates universal dispatcher of QEval.
; Depends on: «defs.scm», «qeval-proc.scm», and overrides.
;
(define (make-qeval-disp simple-query qprocs)
 (define (find-qproc name tail)
  (cond
   ((null? tail) '())
   ((eq? name (caar tail))
    (cdar tail)
   )
   (else
    (find-qproc name (cdr tail))
   )
  )
 )

 (lambda (pattern frame-stream)
  (define query (untag pattern))
  (define qproc (find-qproc (car query) qprocs))

  (if (null? qproc)
   (simple-query pattern frame-stream)
   (qproc (cdr query) frame-stream)
  )
 )
)
