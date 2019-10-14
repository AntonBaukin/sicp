(define (log . args) (for-each display args) (newline))

; We renamed the mystery arguments and inner iterator
; to show that it's a reverse function. It's effective
; agains recursive variant as of tail-call optimization.
; But it alters the original list!
;
(define (mistery lst)
 (define (reverse-iter lst res)
  (if (null? lst) res
   (let ((tail (cdr lst)))
    (set-cdr! lst res)
    (reverse-iter tail lst)
   )
  )
 )

 (reverse-iter lst '())
)

(define v '(a b c d))
(log "initial list v = " v)

; List w is now a reverse, and original reference
; v is now the ending pair of that list (a . ()).
(define w (mistery v))
(log "mistery list w = " w)
(log "and now list v = " v)
