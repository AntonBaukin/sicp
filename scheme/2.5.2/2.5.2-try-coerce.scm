
; Extension point of try-coerce(). Overwrite it
; to apply else algoritm, such as types tower
; from task 2.84. The arguments are:
;  – type  the target type symbol;
;  – t     type tag symbol of the value;
;  – v     the value (unwrapped).
;
; Returns the value wrapped, or '().
;
(define (try-coerce-to type t v)
 (let ((t->type (get-coercion (list t type))))
  (if (null? t->type) '()
   (t->type v)
  )
 )
)

; General version of try-coerce() with more than
; arguments. Used in tasks 2.82 and 2.84.
(define (try-coerce types values)
 (define (coerce-safe type t v)
  (if (eq? type t)
   (num-tag-set type v) ;<— the result must be wrapped
   (try-coerce-to type t v)
  )
 )

 (define (coerce-all type)
  (define (coerce-iter res ts vs)
   (if (null? ts) res
    (let ((v (coerce-safe type (car ts) (car vs))))
     (if (null? v) '()
      (coerce-iter (cons v res) (cdr ts) (cdr vs))
     )
    )
   )
  )

  (reverse (coerce-iter '() types values))
 )

 (define (try-iter ts)
  (if (null? ts) '()
   (let ((res (coerce-all (car ts))))
    (if (null? res)
     (try-iter (cdr ts))
     res
    )
   )
  )
 )

 (define (same-types? types)
  (if (null? (cdr types)) #t
   (if (eq? (car types) (cadr types))
    (same-types? (cdr types))
    #f ;<— two heading types do not match
   )
  )
 )

 ; We can't coerce all the same types as the result
 ; will be the same that causes infinite recursion!
 (if (same-types? types) '()
  (let ((res (try-iter types)))
   (if (null? res)
    (error "Can't coerce n-arguments" types)
    res
   )
  )
 )
)
