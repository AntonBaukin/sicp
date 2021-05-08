
(define qproc-and-optimized
 (
  (lambda () ;<— immediately invoked function
   (define (match-binding result x xyz)
    (define b (find-binding xyz (car x)))

    (cond
     ; Variable is not found, just append it:
     ((null? b) (cons x xyz))

     ; Found and is not equal?
     ((not (equal? x b)) '())

     ; Found the same value, nothing to change:
     (else xyz)
    )
   )

   (define (match-bindings result a b)
    (if (null? a)
     (append result b)
     (let ((m-result (match-binding result (car a) b)))
      (if (null? m-result) '()
       (match-bindings m-result (cdr a) b)
      )
     )
    )
   )

   ; Matches two frames on consistency. This operation is n².
   ; It gives no benefits when replaces pattern matching.
   (define (match-frames a b)
    (define bindings (match-bindings '() (cdr a) (cdr b)))
    (if (null? bindings) '() (make-frame bindings))
   )

   (define (match-stream a-frame stream)
    (stream-filter
     (lambda (b-frame)
      (define ab (match-frames a-frame b-frame))
      (if (null? ab) #f ab)
     )
     stream
    )
   )

   (define (match-two a b)
    (stream-flatmap
     (lambda (frame)
      (match-stream frame b)
     )
     a
    )
   )

   (define (match-rest result rest)
    (if (null? rest) result
     (match-rest
      (match-two result (car rest))
      (cdr rest)
     )
    )
   )

   (define (match-streams frame-streams)
    (match-rest (car frame-streams) (cdr frame-streams))
   )

   (define (qproc-and-opt conjuncts frame-stream)
    (match-streams
     (map
      (lambda (query)
       (qeval-disp
        (make-pattern query)
        frame-stream
       )
      )
      conjuncts
     )
    )
   )

   (set! qproc-and qproc-and-opt)
  )
 )
)
