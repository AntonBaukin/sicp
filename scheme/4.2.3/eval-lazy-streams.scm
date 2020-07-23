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

 (define (list->native l)
  (if (null? l) '()
   (cons$ (car l) (list->native (cdr l)))
  )
 )

 ; The main trick with task 33 is to call
 ; this function from special form.
 (define (native->pairs p)
  (if (pair$? p)
   (cons
    (native->pairs (car$ p))
    (native->pairs (cdr$ p))
   )
   p
  )
 )

 (define (slice n l)
  (cond
   ((null? l) '())
   ((= 0 n) '())
   (else
    (cons$
     (car l)
     (slice (- n 1) (cdr l))
    )
   )
  )
 )

 ; Ass «apply» form takes native list of arguments,
 ; we have to create cross native-lazy stream ops.
 (define (car-lists$ lists)
  (if (null? lists) '()
   (cons$
    (car (car$ lists))
    (car-lists$ (cdr$ lists))
   )
  )
 )

 (define (cdr-lists$ lists)
  (if (null? lists) '()
   (cons$
    (cdr (car$ lists))
    (cdr-lists$ (cdr$ lists))
   )
  )
 )

 (define (map-lists proc lists)
  (if (null? (car$ lists)) '()
   (cons
    (apply proc (car-lists$ lists))
    (map-lists proc (cdr-lists$ lists))
   )
  )
 )

 (define (map proc . lists)
  (map-lists proc lists)
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
 (global list->native list->native)
 (global native->pairs native->pairs)
 (global slice slice)
 (global map map)
)
