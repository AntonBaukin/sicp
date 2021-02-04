
; Converts variables «?some» of input query
; into pairs of (? . some).
(define (parse-query query)
 (map-symbols query
  (lambda (q)
   (define s (symbol->string q))

   (if (equal? "?" (substring s 0 1))
    (cons '?
     (string->symbol
      (substring s 1 (string-length s))
     )
    )
    q ;<— do not transform
   )
  )
 )
)

(define (map-symbols query mapper)
 (cond
  ((pair? query)
   (cons
    (map-symbols (car query) mapper)
    (map-symbols (cdr query) mapper)
   )
  )

  ((symbol? query)
   (mapper query)
  )

  (else query)
 )
)

; Opposite to «parse-query» utility.
; Replaces pairs (? . xyz) with symbol «?xyz».
(define (print-query parsed)
 (cond
  ((variable? parsed)
   (string->symbol
    (string-append "?"
     (symbol->string (variable-name parsed))
    )
   )
  )
  ((pair? parsed)
   (cons
    (print-query (car parsed))
    (print-query (cdr parsed))
   )
  )
  (else parsed)
 )
)

; Creates a new frame by extending the given one
; with the (name . value) variable binding.
(define (frame-bind frame name value)
 (make-frame
  (cons
   (make-binding name value)
   (frame-bindings frame)
  )
 )
)

; Searches for binding with the given name.
; Returns empty list, if not found.
(define (find-binding bindings name)
 (cond
  ((null? bindings) '())
  ((eq? name (binding-name (car bindings))) (car bindings))
  (else (find-binding (cdr bindings) name))
 )
)

(define (frame-get frame name)
 (find-binding (frame-bindings frame) name)
)

; Used for testing.
(define (frame->list frame)
 (define (next res bindings)
  (if (null? bindings)
   (reverse res) ;<— natural order
   (next
    (cons
     (list (caar bindings) (cdar bindings))
     res
    )
    (cdr bindings)
   )
  )
 )

 (next '() (frame-bindings frame))
)

; Takes parsed query (untagged pattern) and the frame,
; and substitutes found variables.
(define (instantiate query frame)
 (cond
  ((variable? query)
   (let ((value (frame-get frame (variable-name query))))
    (if (null? value)
     (print-query query)   ;<— return variable as «?name»
     (binding-value value) ;<— return that value
    )
   )
  )

  ((pair? query)
   (cons
    (instantiate (car query) frame)
    (instantiate (cdr query) frame)
   )
  )

  (else query)
 )
)

(define (exp-depends-on? exp var frame)
 (define (iter e)
  (cond
   ((variable? e)
    (if (equal? var e) #t
     (let ((b (frame-get frame (variable-name e))))
      (if (null? b) #f (iter (binding-value b)))
     )
    )
   )
   ((pair? e) (or (iter (car e)) (iter (cdr e))))
   (else #f)
  )
 )

 (iter exp)
)

(define unique-var-id 1)

(define (next-unique-var-id)
 (define result unique-var-id)
 (set! unique-var-id (+ unique-var-id 1))
 result
)

(define (make-unique-var id var)
 (cons '?
  (string->symbol
   (string-append
    "$" (number->string id) ":"
    (symbol->string (variable-name var))
   )
  )
 )
)

(define (rename-vars-in id exp)
 (define (iter exp)
  (cond
   ((variable? exp)
    (make-unique-var id exp)
   )

   ((pair? exp)
    (cons
     (rename-vars-in id (car exp))
     (rename-vars-in id (cdr exp))
    )
   )

   (else exp)
  )
 )

 (iter exp)
)

; Assuming that there is no cyclic dependencies
(define resolve-all-vars
 (
  (lambda () ;<— immediately invoked function

   (define (resolve-first head tail)
    (if (null? tail) head
     (let ((b (resolve-binding (append head tail) (car tail))))
      (if (null? b)
       (resolve-first (cons (car tail) head) (cdr tail))
       ; After replacement we start again to back-resolve:
       (resolve-first '() (append (cons b head) (cdr tail)))
      )
     )
    )
   )

   (define (resolve-binding bindings b)
    (define x (resolve-value bindings (binding-value b)))
    (if (equal? x (binding-value b)) '()
     (cons (binding-name b) x)
    )
   )

   (define (resolve-value bindings value)
    (cond
     ((variable? value)
      (let ((b (find-binding bindings (variable-name value))))
       (if (null? b) value (binding-value-unlist b))
      )
     )
     ((pair? value)
      (cons
       (resolve-value bindings (car value))
       (resolve-value bindings (cdr value))
      )
     )
     (else value)
    )
   )

   (lambda (bindings)
    (resolve-first '() bindings)
   )
  )
 )
)
