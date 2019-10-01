
; Read task 2.84 for the details.
(define (try-raise-up-to type value)
 (let ((raised (raise-safe value)))
  (if (null? raised) '()
   (if (eq? type (num-tag-get raised))
    raised
    (try-raise-up-to type raised)
   )
  )
 )
)

; Overwrite the extension point of 2.82 task. First,
; we try the directly registered coercion, then we
; try to raise it up.
(define (try-coerce-to type t v)
 (let ((t->type (get-coercion (list t type))))
  (if (null? t->type)
   (try-raise-up-to type (num-tag-set t v))
   (t->type v)
  )
 )
)