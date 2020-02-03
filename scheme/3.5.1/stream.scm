
; This implementation is specific for Gambit Scheme,
; that has no special form «cons-stream».
(define-macro (cons-stream item delayed-expr)
 `(cons ,item (delay ,delayed-expr))
)

(define (stream-pair? x)
 (and
  (pair? x)
  (promise? (cdr x))
 )
)

(define (stream-car s)
 (car s)
)

(define (stream-cdr s)
 (force (cdr s))
)

(define the-empty-stream '())

(define stream-null? null?)

(define (stream-length s)
 (if (stream-null? s) 0
  (+ 1 (stream-length (stream-cdr s)))
 )
)

(define (stream-ref stream i)
 (if (= i 0)
  (stream-car stream)
  (stream-ref (stream-cdr stream) (- i 1))
 )
)

(define (stream->list s)
 (if (stream-null? s) '()
  (cons
   (stream-car s)
   (stream->list (stream-cdr s))
  )
 )
)

(define (sub-stream->list n s)
 (cond
  ((stream-null? s) '())
  ((= 0 n) '())
  (else
   (cons
    (stream-car s)
    (sub-stream->list (- n 1) (stream-cdr s))
   )
  )
 )
)

(define (list->stream l)
 (if (null? l) the-empty-stream
  (cons-stream (car l) (list->stream (cdr l)))
 )
)

; Concatenates finite «first» stream
; with «then» one that may be infinite.
(define (stream-concat first then)
 (if (stream-null? first) then
  (cons-stream
   (stream-car first)
   (stream-concat (stream-cdr first) then)
  )
 )
)

; Advanced version of a stream filter.
; If check predicate returns #t — passes item
; to the resulting stream. If check transform
; returns a list — passes all items to result.
;
(define (stream-filter check stream)
 (if (stream-null? stream) the-empty-stream
  (let ((x (check (stream-car stream))))
   (cond
    ((eq? #t x)
     (cons-stream
      (stream-car stream)
      (stream-filter check (stream-cdr stream))
     )
    )

    ((list? x)
     (cons-stream
      (car x)
      (stream-concat
       (list->stream (cdr x))
       (stream-filter check (stream-cdr stream))
      )
     )
    )

    (else (stream-filter check (stream-cdr stream)))
   )
  )
 )
)

(define (stream-find-index stream predicate)
 (define (next i s)
  (if (predicate (stream-car s)) i
   (next (+ i 1) (stream-cdr s))
  )
 )

 (next 0 stream)
)

(define (any-stream-null? streams)
 (cond
  ((null? streams) #f)
  ((stream-null? (car streams)) #t)
  (else (any-stream-null? (cdr streams)))
 )
)

(define (stream-map mapper . streams)
 (if (any-stream-null? streams)
  the-empty-stream
  (cons-stream
   (apply mapper (map stream-car streams))
   (apply stream-map (cons mapper (map stream-cdr streams)))
  )
 )
)
