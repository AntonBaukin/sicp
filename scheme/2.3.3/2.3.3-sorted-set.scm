(define (log . args) (for-each display args) (newline))

; Creates collection of utilities related to sorted set
; packed into a single ops-list, i.e. an «object».
; Use set-op-* selectors of the ops.
; Comparator is as < operator.
(define (make-sorted-set smaller?)

 (define (sorted? set)
  (cond
   ((null? set) #t)
   ((null? (cdr set)) #t)
   ((not (smaller? (car set) (cadr set))) #f)
   (else (sorted? (cdr set)))
  )
 )

 (define (add set x)
  (cond
   ((null? set) (list x))

   ((smaller? x (car set)) (cons x set))
   
   ((smaller? (car set) x)
    (cons (car set) (add (cdr set) x))
   )

   (else set) ;<— item duplicates
  )
 )

 (define (make items)
  (define (next res tail)
   (if (null? tail) res
    (next (add res (car tail)) (cdr tail))
   )
  )

  (if (sorted? items) items (next '() items))
 )

 (define (has? set x)
  (cond
   ((null? set) #f)

   ((smaller? x (car set)) #f)

   ((smaller? (car set) x)
    (has? (cdr set) x)
   )

   (else #t) ; as not < and not >
  )
 )

 (define (intersect seta setb)
  (cond
   ((null? seta) '())
   ((null? setb) '())

   ((smaller? (car seta) (car setb))
    (intersect (cdr seta) setb)
   )

   ((smaller? (car setb) (car seta))
    (intersect seta (cdr setb))
   )

   ; not < and not > means = to take single of equal items
   (else (cons (car seta) (intersect (cdr seta) (cdr setb))))
  )
 )

 (define (union seta setb)
  (cond
   ((null? seta) setb)
   ((null? setb) seta)

   ((smaller? (car seta) (car setb))
    (cons (car seta) (union (cdr seta) setb))
   )

   ((smaller? (car setb) (car seta))
    (cons (car setb) (union seta (cdr setb)))
   )

   ; not < and not > means = to take single of equal items
   (else (cons (car seta) (union (cdr seta) (cdr setb))))
  )
 )

 (list add has? make sorted? intersect union)
)

(define (set-op-add setops)
 (list-ref setops 0)
)

(define (set-op-has? setops)
 (list-ref setops 1)
)

; Make set op takes argitrary list of items:
; not sorted and with duplicates.
(define (set-op-make setops)
 (list-ref setops 2)
)

(define (set-op-sorted? setops)
 (list-ref setops 3)
)

(define (set-op-intersect setops)
 (list-ref setops 4)
)

(define (set-op-union setops)
 (list-ref setops 5)
)
