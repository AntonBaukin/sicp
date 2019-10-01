(include "2.5.2-arithmetics.scm")
(include "2.5.2-try-coerce.scm")
(include "2.5.2-tower.scm")
(include "2.5.2-raise.scm")

(define (log . args) (for-each display args) (newline))

(define I make-integer) ;<— shortcuts...
(define N make-number)

; This call throws error as the initial version
; can't infer float number from integer with
; transitive rational.
;(log "1 + 2. = " (add (I 1) (N 2)))

; We solve this problem using raise(). We have no enough
; metadata in the apply-generic registration table to
; predict that the target type is above in the tower.
; But we may raise the given value up till the end.
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

; And now we are able to make this addition.
(log "1 + 2. = " (add (I 1) (N 2)))
