(define (log . args) (for-each display args) (newline))

; Creates set eq-checking the items: if item
; is absent on add, adds it and returns true.
(define (make-identity-set)
 (define set '())

 (define (contains? tail item)
  (if (null? tail) #f
   (if (eq? item (car tail)) #t
    (contains? (cdr tail) item)
   )
  )
 )

 (lambda (item)
  (if (contains? set item) #f
   (begin
    (set! set (cons item set))
    #t
   )
  )
 )
)

(define (looped? lst)
 (define add? (make-identity-set))

 (define (iter tail)
  (cond
   ((null? tail) #f)
   ((not (add? (car tail))) #t)
   (else (iter (cdr tail)))
  )
 )

 (iter lst)
)

(define a '(a b c d e))
(log "looped " a " ?= " (looped? a))

(define b '(a b c d e))
; Now point the tail to the head:
(set-cdr! (cddddr b) b)
(log "looped " b " ?= " (looped? b))
