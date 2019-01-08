(define (log . args) (for-each display args) (newline))

(define nil (list))

(define (square-list-reversed items)
 (define (iter tail res)
  (if (null? tail) res
   (iter (cdr tail)
    ; inserts each following item as the first
    (cons (square (car tail)) res)
   )
  )
 )

 (iter items nil)
)

(define (square-list-wrong items)
 (define (iter tail res)
  (if (null? tail) res
   (iter (cdr tail)
    (cons res (square (car tail)))
   )
  )
 )

 (iter items nil)
)

(define (square-list-normal items)
 (define (iter tail res)
  (if (null? tail) res
   (iter (cdr tail)
    ; to reverse the result is faster — o(n)
    (append res (list (square (car tail))))
   )
  )
 )

 (iter items nil)
)

(log "reversed square of (1 2 3 4) is " (square-list-reversed (list 1 2 3 4)))
(log "wrong square of (1 2 3 4) is " (square-list-wrong (list 1 2 3 4)))
(log "normal square of (1 2 3 4) is " (square-list-normal (list 1 2 3 4)))
