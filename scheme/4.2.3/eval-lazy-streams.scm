(eval-basic
 (define (get-car) void)
 (define (get-cdr) void)
 (define (is-pair) void)

 (define (disp q x y)
  (cond
   ((eq? q get-car) x)
   ((eq? q get-cdr) y)
   ((eq? q is-pair) #t)
  )
 )

 (define (cons x y)
  (lambda (q) (disp q x y))
 )

 (define (car p)
  (p get-car)
 )

 (define (cdr p)
  (p get-cdr)
 )

 ; Note that '() is not a pair, but is a list.
 (define (pair? p)
  (and
   (procedure? p)
   ; We have to involve «try» form as we can't
   ; test arbitrary procedure as some runtime
   ; type identification...
   (try (p is-pair) #f)
  )
 )

 (define (make-list items)
  (if (null? items) '()
   (cons (car$ items) (make-list (cdr$ items)))
  )
 )

 (define (list . items)
  (make-list items)
 )

 (define (list? some)
  (or
   (null? some) ;<— unlike pair?
   (and
    (pair? some)
    (list? (cdr some))
   )
  )
 )

 (define (length-next n tail)
  (if (null? tail) n
   (length-next (+ n 1) (cdr tail))
  )
 )

 (define (length l)
  (length-next 0 l)
 )

 (define (list-ref l i)
  (if (= 0 i) (car l) (list-ref (cdr l) (- i 1)))
 )

 (define (lists-equal? a b)
  (cond
   ((null? a) (null? b))
   ((null? b) #f)
   (else
    (if (equal? (car a) (car b))
     (lists-equal? (cdr a) (cdr b))
     #f
    )
   )
  )
 )

 (define (map proc l)
  (if (null? l) '()
   (cons (proc (car l)) (map proc (cdr l)))
  )
 )


 ; Define global functions:
 (global cons cons)
 (global car car)
 (global cdr cdr)
 (global pair? pair?)
 (global list list)
 (global list? list?)
 (global length length)
 (global list-ref list-ref)
 (global lists-equal? lists-equal?)
 (global map map)
)
