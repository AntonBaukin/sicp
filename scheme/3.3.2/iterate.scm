
; Invokes visitor callback for each item of the list.
; If visitor returns value #f, breaks the iteration and
; returns the break item (find behaviour).
(define (iterate-list lst visitor)
 (define (next tail)
  (cond
   ((null? tail) void)
   ((eq? #f (visitor (car tail)))
    (car tail)
   )
   (else (next (cdr tail)))
  )
 )

 (next lst)
)

; Creates iteration function having no arguments.
; On each call the function returns whether an item
; of the list in the same order, or empty list.
(define (list-iterator lst)
 (lambda ()
  (if (null? lst) '()
   (let ((result (car lst)))
    (set! lst (cdr lst))
    result
   )
  )
 )
)

(define (list-iterator-ext null-value lst)
 (lambda ()
  (if (null? lst) null-value
   (let ((result (car lst)))
    (set! lst (cdr lst))
    result
   )
  )
 )
)

; Collectes iterated items to a list.
(define (iterator->list it)
 (define (next result)
  (define item (it))

  (if (null? item)
   (reverse result)
   (next (cons item result))
  )
 )

 (next '())
)

(define (iterator->list-ext isnull? it)
 (define (next result)
  (define item (it))

  (if (isnull? item)
   (reverse result)
   (next (cons item result))
  )
 )

 (next '())
)

; Creates iterator that applies given transformation
; for original items iterated. Treats empty list as stop.
(define (it-transform it transform)
 (lambda ()
  (define item (it))
  (if (null? item) '() (transform item))
 )
)

; Performs composite iteration over several collections.
; Super iterator returns collections, and «make-sub-it»
; returns sub-iterator for given collection.
(define (join-iterators-ext null-value isnull? super-it make-sub-it)
 (define coit '())

 (define (next)
  (if (null? coit)
   (let ((co (super-it)))
    (if (isnull? co)
     null-value ;<— done the iteration
     (begin
      (set! coit (make-sub-it co))
      (next)
     )
    )
   )
   (let ((item (coit)))
    (if (isnull? item)
     (begin
      (set! coit '())
      (next)
     )
     item
    )
   )
  )
 )

 next ;<— resulting iterator
)

(define (join-iterators super-it make-sub-it)
 (join-iterators-ext '() null? super-it make-sub-it)
)
