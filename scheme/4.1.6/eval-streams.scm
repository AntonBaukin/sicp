
; Copy some of «3.5.1/stream.scm» and «3.5.1/streams.scm».
; The tests are from «3.5.1/streams-test.scm».
(eval-basic
 (define (promise? x)
  (and
   (list? x)
   (= 3 (length x))
   (eq? 'promise (car x))
  )
 )

 (define (resolved-promise? x)
  (and
   (list? x)
   (= 3 (length x))
   (eq? 'promise (car x))
   (eq? 'resolved (cadr x))
  )
 )

 (define (get-resolved-value promise)
  (caddr promise)
 )

 (define (resolve-promise p)
  (define result (eval-promise p))

  ; Memoize the value in the same record:
  (set-car! (cdr p) 'resolved)
  (set-car! (cddr p) result)

  result
 )

 ; Evaluates a promise in it's environment.
 ; Resulting value is memoized.
 (define (force p)
  (cond
   ((resolved-promise? p)
    (get-resolved-value p)
   )

   ((promise? p)
    (resolve-promise p)
   )

   (else (error "Not a promise to force" p))
  )
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

 ; Produces stream of integers [a; b], «b» — included.
 (define (stream-enumerate-range a b)
  (if (> a b)
   the-empty-stream
   (cons-stream a (stream-enumerate-range (+ a 1) b))
  )
 )

 ; Produces n-length ctream of: a, a + step, a + 2*step,..
 (define (stream-enumerate-stepped-n step n a)
  (if (<= n 0)
   the-empty-stream
   (cons-stream a (stream-enumerate-stepped-n step (- n 1) (+ a step)))
  )
 )

 (define (integers-stream from)
  (cons-stream from (integers-stream (+ 1 from)))
 )

 (define (stream-of value)
  (define s (cons-stream value s))
  s ;<— resulting stream
 )

 (define (add-streams . streams)
  (apply stream-map (cons + streams))
 )

 (define (mul-streams . streams)
  (apply stream-map (cons * streams))
 )

 (define (scale-stream number stream)
  (mul-streams (stream-of number) stream)
 )


 ; Define global functions:
 (global promise? promise?)
 (global force force)
 (global stream-pair? stream-pair?)
 (global stream-car stream-car)
 (global stream-cdr stream-cdr)
 (global the-empty-stream the-empty-stream)
 (global stream-null? stream-null?)
 (global stream-length stream-length)
 (global stream-ref stream-ref)
 (global stream->list stream->list)
 (global sub-stream->list sub-stream->list)
 (global list->stream list->stream)
 (global list->stream list->stream)
 (global stream-concat stream-concat)
 (global stream-filter stream-filter)
 (global stream-find-index stream-find-index)
 (global stream-map stream-map)
 (global stream-enumerate-range stream-enumerate-range)
 (global stream-enumerate-stepped-n stream-enumerate-stepped-n)
 (global integers-stream integers-stream)
 (global stream-of stream-of)
 (global add-streams add-streams)
 (global mul-streams mul-streams)
 (global scale-stream scale-stream)
)
