; Depends on «../3.3.2/iterate.scm».

;
; Warning! This module, as whole QEval in this part,
; does use «void» value as the iteration stop-signal
; instead of the empty lists.
;

;
; Copy from «3.5.1/stream.scm» and «3.5.1/streams.scm»
; with functions specific for Queries Evaluator (QEval).
;
(define-macro (cons-stream item delayed-expr)
 `(cons ,item (delay ,delayed-expr))
)

(define the-empty-stream '())

(define stream-null? null?)

(define (void? x) (eq? void x))

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

(define (stream-flatmap mapper stream)
 (stream-flatten (stream-map mapper stream))
)

; Does 1-level flattenning of the items that must be streams.
; The resulting items are interleaved.
(define (stream-flatten stream)
 (if (stream-null? stream)
  the-empty-stream
  (interleave-delayed
   (stream-car stream)
   (delay (stream-flatten (stream-cdr stream)))
  )
 )
)

(define (interleave-delayed sa sb-delayed)
 (if (stream-null? sa)
  (force sb-delayed)
  (cons-stream
   (stream-car sa)
   (interleave-delayed
    (force sb-delayed)
    (delay (stream-cdr sa))
   )
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

; HOF that wraps iterator stopping on empty list
; to iterator stopping on void.
(define (voided-iterator it)
 (lambda ()
  (define v (it))
  (if (null? v) void v)
 )
)

; Takes iterator and returns a stream over it.
; Iterator is a function that returns an item on each call,
; or an empty list, when it's done. Transformation function
; takes an item and returns a value.
(define (iterator->stream it)
 (define (next)
  (define item (it))

  (if (void? item)
   the-empty-stream
   (cons-stream item (next))
  )
 )

 (next)
)

; Creates composite iterator via «join-iterators» wrapped as a stream.
(define (join-iterator->stream super-it make-sub-it)
 (iterator->stream (join-iterators-ext void void? super-it make-sub-it))
)

(define (stream-append a b)
 (if (stream-null? a)
  b
  (cons-stream
   (stream-car a)
   (stream-append (stream-cdr a) b)
  )
 )
)

(define (stream-filter matcher stream)
 (if (stream-null? stream)
  the-empty-stream
  (let ((x (matcher (stream-car stream))))
   (cond
    ((eq? #t x)
     (cons-stream
      (stream-car stream)
      (stream-filter matcher (stream-cdr stream))
     )
    )
    ((eq? #f x)
     (stream-filter matcher (stream-cdr stream))
    )
    (else
     (cons-stream
      x ;<— use match result as a value
      (stream-filter matcher (stream-cdr stream))
     )
    )
   )
  )
 )
)

(define (singleton-stream item)
 (cons-stream item the-empty-stream)
)
