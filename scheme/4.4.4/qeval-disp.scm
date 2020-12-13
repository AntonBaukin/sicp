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

(define (qeval-disp-impl pattern frame-stream)
 (define query (untag pattern))
 (define qproc (find-qproc (car query) qeval-procs))

 (if (null? qproc)
  (simple-query pattern frame-stream)
  (qproc (cdr query) frame-stream)
 )
)

(define set-qeval-disp
 (
  (lambda () ;<— immediately invoked function
   (set! qeval-disp qeval-disp-impl)
  )
 )
)
