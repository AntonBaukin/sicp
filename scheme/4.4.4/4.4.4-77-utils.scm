(define FRAME_PROMISE '$promise)

(define (frame-get-promise frame)
 (define b (frame-get frame FRAME_PROMISE))
 (if (null? b) '() (binding-value b))
)

(define (frame-add-promise frame op)
 (frame-bind frame FRAME_PROMISE
  (append (frame-get-promise frame) (list op))
 )
)

(define (frame-del-promise frame op)
 (define (del result rest)
  (cond
   ((null? rest) result)
   ((eq? op (car rest))
    (append result (cdr rest))
   )
   (else
    (del (cons (car rest) result) (cdr rest))
   )
  )
 )

 (frame-bind frame FRAME_PROMISE
  (del '() (frame-get-promise frame))
 )
)

(define (frame-resolve-promise-ops frame ops)
 (define (resolve fops ops)
  (if (null? ops) fops
   (let ((x ((car ops) (car fops))))
    (cond
     ((null? x) '())

     ((eq? #f x)
      (resolve
       (cons (car fops) (cons (car ops) (cdr fops)))
       (cdr ops)
      )
     )

     (else
      (resolve
       (cons x (cdr fops))
       (cdr ops)
      )
     )
    )
   )
  )
 )

 (define fops (resolve
  ; Unbind current ops not to cause infinite loop:
  (cons (frame-unbind frame FRAME_PROMISE) '())
  ops
 ))

 (cond
  ((null? fops) '())

  ((null? (cdr fops))
   (frame-unbind (car fops) FRAME_PROMISE)
  )

  (else
   (frame-bind (car fops) FRAME_PROMISE (cdr fops))
  )
 )
)

(define (frame-resolve-promise frame)
 (define ops (frame-get-promise frame))
 (if (null? ops) frame (frame-resolve-promise-ops frame ops))
)

(define (collect-pattern-vars pattern)
 (define (next result some)
  (cond
   ((null? some) result)

   ((variable? some)
    (cons
     (variable-name some)
     result
    )
   )

   ((pair? some)
    (next
     (next result (car some))
     (cdr some)
    )
   )

   (else result)
  )
 )

 (next '() pattern)
)

(define (pattern-resolved? pattern frame)
 (define vars (collect-pattern-vars pattern))

 (define (next rest)
  (if (null? rest) #t
   (let ((b (frame-get frame (car rest))))
    (if (null? b) #f
     (next (cdr rest))
    )
   )
  )
 )

 (next vars)
)

(define (frame-resolve-filter frame)
 (define frame-resolved (frame-resolve-promise frame))
 (if (null? frame-resolved) #f frame-resolved)
)
