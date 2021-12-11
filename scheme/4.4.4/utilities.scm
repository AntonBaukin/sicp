; Treats values of bindings: if the value is 1-element list,
; returns element at zero, else — returns the value.
(define (binding-value-unlist binding)
 (define v (binding-value binding))
 (if (and (list? v) (= 1 (length v))) (car v) v)
)

(define (extend-binding binding ext-list)
 (make-binding-ext (binding-name binding) (binding-value binding) ext-list)
)

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
; with the (name value . ext) variable binding.
(define (frame-bind frame name value . ext)
 (define (rebind result rest)
  (cond
   ((null? rest)
    (cons
     (make-binding-ext name value ext)
     result
    )
   )

   ((eq? name (binding-name (car rest)))
    (append
     result
     (list (make-binding-ext name value ext))
     (cdr rest)
    )
   )

   (else
    (rebind (cons (car rest) result) (cdr rest))
   )
  )
 )

 (extend-frame frame
  (rebind '() (frame-bindings frame))
 )
)

(define (frame-unbind frame name)
 (define (unbind result rest)
  (cond
   ((null? rest) result)

   ((eq? name (binding-name (car rest)))
    (append result (cdr rest))
   )

   (else
    (unbind (cons (car rest) result) (cdr rest))
   )
  )
 )

 (extend-frame frame
  (make-frame (unbind '() (frame-bindings frame)))
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

; Searches for binding within the frame's own bindings
; not going up to the frame ancestors.
(define (frame-get frame name)
 (if (null? frame) '()
  (find-binding (frame-bindings frame) name)
 )
)

; Same as frame-get, but goes up the stack.
(define (frame-get-up frame name)
 (define result (frame-get frame name))

 (if (not (null? result)) result
  (let ((parent (frame-parent frame)))
   (if (null? parent) '()
    ; WARNING: under consideration: parent Vs any ancestor?
    ; (frame-get-up (frame-parent frame) name)
    (frame-get (frame-parent frame) name)
   )
  )
 )
)

(define (frame-ancestor frame level)
 (cond
  ((null? frame) '())
  ((= level (frame-level frame)) frame)
  (else (frame-ancestor (frame-parent frame) level))
 )
)

(define (frame-set-ancestor frame ancestor)
 (cond
  ((null? frame) frame)
  ((= (frame-level frame) (+ 1 (frame-level ancestor)))
   (frame-set-parent frame ancestor)
  )
  (else
   (frame-set-parent frame
    (frame-set-ancestor (frame-parent) frame ancestor)
   )
  )
 )
)

(define (frame-get-at frame level name)
 (frame-get (frame-ancestor frame level) name)
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

(define (frame-top-parent frame)
 (if (null? (frame-parent frame))
  frame
  (frame-top-parent (frame-parent frame))
 )
)

; Takes parsed query (untagged pattern) and the frame,
; and substitutes found variables.
(define (instantiate query frame)
 (instantiate-impl query (frame-top-parent frame))
)

(define (instantiate-impl query frame)
 (cond
  ((variable? query)
   (let ((b (frame-get frame (variable-name query))))
    (if (null? b)
     (print-query query)                        ;<— return variable as «?name»
     (instantiate-impl (binding-value b) frame) ;<— resolve value recursively
    )
   )
  )

  ((pair? query)
   (cons
    (instantiate-impl (car query) frame)
    (instantiate-impl (cdr query) frame)
   )
  )

  (else query)
 )
)

(define (prepare-for-eval value)
 (define (ps v)
  ; We quote pairs not to treat them as function calls:
  (if (pair? v) (list 'quote v) v)
 )

 (append
  ; First item is function name on global scope:
  (list (car value))
  ; Each following item is a parameter of the call:
  (map ps (cdr value))
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

(define (string-search string char)
 (define l (string-length string))

 (define (next i)
  (cond
   ((= i l) -1)
   ((eq? char (string-ref string i)) i)
   (else (next (+ i 1)))
  )
 )

 (next 0)
)

; Takes name of unique var of form $N:name and
; returns a pair of (name:string . N:number),
; or null, if name given is not a unique one.
(define (decode-unique-var name-sym)
 (define name (symbol->string name-sym))

 (if (not (eq? #\$ (string-ref name 0))) '()
  (let ((i (string-search name #\:)))
   (if (= -1 i) '()
    (cons
     (substring name (+ i 1) (string-length name))
     (string->number (substring name 1 i))
    )
   )
  )
 )
)

; Generates unique id on the first demand.
(define (rename-vars-in make-id exp)
 (define id '())

 (define (get-id)
  (if (null? id)
   (set! id (make-id))
  )
  id
 )

 (define (iter exp)
  (cond
   ((variable? exp)
    (make-unique-var (get-id) exp)
   )

   ((pair? exp)
    (cons
     (iter (car exp))
     (iter (cdr exp))
    )
   )

   (else exp)
  )
 )

 (iter exp)
)

; Assuming that there is no cyclic dependencies, substitutes
; each variable-referring value with the value of that variable.
; Continues till no further replacement is possible.
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
