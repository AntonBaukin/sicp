;
; Table is a list of pairs (key . value).
; To compare keys on equality keys-eq? is used.
;
(define (make-table keys-eq?)
 (define (make) '())

 (define (search tail key)
  (if (null? tail) void
   (if (keys-eq? key (caar tail))
    (car tail) ;<— return (key . value)
    (search (cdr tail) key)
   )
  )
 )

 (define (save table key value)
  (cons (cons key value) table)
 )

 (define (rewrite table kv value)
  (set-cdr! kv value)
  table
 )

 (define (delete-key key head tail)
  (cond
   ((null? tail) void)

   ((keys-eq? (caar tail) key) ;<— key match
    (append (reverse head) (cdr tail))
   )

   (else
    (delete-key key (cons (car tail) head) (cdr tail))
   )
  )
 )

 (define (without table key)
  (let ((r (delete-key key '() table)))
   ; If key was not found — return source table:
   (if (eq? void r) table r)
  )
 )

 ; Here kvx is a list of (k v (k . v)).
 (define (iter-set kvx value)
  (define kv (caddr kvx))
  (set-cdr! kv value)
 )

 (define (iter table)
  (define it (list-iterator table))
  ; We cache «kvs» instance as it's allowed by the interface:
  (define x (cons '() '()))
  (define vx (cons '() x))
  (define kvx (cons '() vx))
  (define kvs (cons kvx iter-set))

  (lambda ()
   (define kv (it))

   (if (null? kv) '()
    (begin
     (set-car! x kv) ;<— save the table pair
     (set-car! vx (cdr kv))
     (set-car! kvx (car kv))
     kvs ;<— always return the same instance
    )
   )
  )
 )

 (make-table-base make search save rewrite without length iter)
)
