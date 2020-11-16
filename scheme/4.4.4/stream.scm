; Depends on «../3.3.2/iterate.scm».

;
; Copy from «3.5.1/stream.scm» and «3.5.1/streams.scm»
; with functions specific for Queries Evaluator (QEval).
;
(define-macro (cons-stream item delayed-expr)
 `(cons ,item (delay ,delayed-expr))
)

(define the-empty-stream '())

(define stream-null? null?)

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

; Unlike general implementation from «3.5.1/stream.scm»
; that takes multiple streams, this version takes one.
(define (stream-map mapper stream)
 (if (stream-null? stream)
  the-empty-stream
  (cons-stream
   (mapper (stream-car stream))
   (stream-map mapper (stream-cdr stream))
  )
 )
)

; Works for fixed length streams. Used mostly for the testing.
(define (stream->list s)
 (if (stream-null? s) '()
  (cons
   (stream-car s)
   (stream->list (stream-cdr s))
  )
 )
)

; Returns list from the leading n-items of maybe infinite stream.
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

(define (produced-stream producer)
 (cons-stream
  (producer)
  (produced-stream producer)
 )
)

; Takes iterator and returns a stream over it.
; Iterator is a function that returns an item on each call,
; or an empty list, when it's done. Transformation function
; takes an item and returns a value.
(define (iterator->stream it)
 (define (next)
  (define item (it))

  (if (null? item)
   the-empty-stream
   (cons-stream item (next))
  )
 )

 (next)
)

; Creates composite iterator via «join-iterators» wrapped as a stream.
(define (join-iterator->stream super-it make-sub-it)
 (iterator->stream (join-iterators super-it make-sub-it))
)

(define (stream-append a b)
 (if (stream-null? a)
  b
  (cons
   (stream-car a)
   (stream-append (stream-cdr a) b)
  )
 )
)
