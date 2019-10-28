
; Composes a list taking the initial list and passing it to
; the following makers. Each maker takes current version
; of this list and produces item list to append.
(define (compose-list initial . makers)
 (define (iter result makers)
  (if (null? makers) result
   (iter
    (append
     result
     ((car makers) result)
    )
    (cdr makers)
   )
  )
 )

 (iter initial makers)
)
