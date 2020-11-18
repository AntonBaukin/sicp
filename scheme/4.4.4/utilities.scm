

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
(define (frame-get frame name)
 (define (next bindings)
  (cond
   ((null? bindings) '())
   ((eq? name (binding-name (car bindings)))
    (car bindings)
   )
   (else (next (cdr bindings)))
  )
 )

 (next (frame-bindings frame))
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
