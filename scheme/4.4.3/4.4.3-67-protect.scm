;
; Implementation of QEval protector.
;
(define (make-qeval-protector make-entry protect-looped?)
 (define stack '())

 (define (push query frame-stream)
  (define entry (make-entry query frame-stream))
  (define looped? (protect-looped? entry stack))

  (if (not looped?)
   (set! stack (cons entry stack))
  )

  looped?
 )

 (define (pop)
  (if (null? stack)
   (error "QEval stack is empty!")
   (set! stack (cdr stack))
  )
 )

 (lambda (case query frame-stream)
  (cond
   ((eq? 'push case)
    (push query frame-stream)
   )

   ((eq? 'pop case)
    (pop)
    frame-stream
   )

   (else (error "Wrong QEval protection case" case))
  )
 )
)
