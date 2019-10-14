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

(define (count-pairs struct)
 (define add? (make-identity-set))

 (define (iter x)
  ; We just extend the condition to check unique:
  (if (not (and (pair? x) (add? x))) 0
   (+ 1 (iter (car x)) (iter (cdr x)))
  )
 )

 (iter struct)
)


; The following tests are from task 3.16:
; Now count-pairs() gives 3 for each of them.
(define w '(a b c))
(log "count " w " = " (count-pairs w))

(define v '(a b c))
(set-car! v (cddr v))
(log "count " v " = " (count-pairs v))

(define y '(a b c))
(set-car! y (cdr y))
(set-car! (cdr y) (cddr y))
(log "count " y " = " (count-pairs y))

; And we even may count for a loop:
(define o '(a b c))
(set-cdr! (cddr o) o)
; Yes, Schema's display() has built-in
; protection agains loops:
(log "count " o " = " (count-pairs o))
