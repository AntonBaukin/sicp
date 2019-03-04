(define (log . args) (for-each display args) (newline))

(include "../2.3.3/tree.scm")

; When called with single argument, checks whether the
; given object is a tag wrapper; with two — that the tag
; is the same as the given.
(define (apply-generic-tagged? obj . tag)
 (and
  (pair? obj)
  (pair? (car obj))
  (eq? 'apply-generic-tagged (caar obj))
  (or (null? tag)
   (if (= 1 (length tag))
    (if (symbol? (car tag))
     (eq? (car tag) (cdar obj))
     (error "Not a tag symbol!" (car tag))
    )
    (error "Only one tag symbol is allowed!" tag)
   )
  )
 )
)

(define (apply-generic-tag-get wrapper)
 (if (apply-generic-tagged? wrapper)
  (cdar wrapper)
  (error "Not apply-generic tagged!" wrapper)
 )
)

; Returns object under tag wrapper.
(define (apply-generic-obj wrapper)
 (if (apply-generic-tagged? wrapper)
  (cdr wrapper)
  (error "Not apply-generic tagged!" wrapper)
 )
)

(define (apply-generic-tag-set tag obj)
 (if (symbol? tag)
  (cons (cons 'apply-generic-tagged tag) obj)
  (error "Not a tag symbol!" tag)
 )
)

; When called with single argument, returns the tag attached
; to the wrapping object. With second argument, assigns the
; tag to the object and returns wrapping object.
(define (apply-generic-tag tag_or_obj . obj)
 (if (null? obj)
  (apply-generic-tag-get tag_or_obj)
  (if (= 1 (length obj))
   (apply-generic-tag-set tag_or_obj (car obj))
   (error "Only one tag symbol is allowed!" obj)
  )
 )
)

(define (symbol-ci<? a b)
 (string-ci<? (symbol->string a) (symbol->string b))
)

(define (symbols-list<? list-a list-b)
 (define (next tail-a tail-b)
  (cond
   ((null? tail-a) (not (null? tail-b)))
   ((null? tail-b) #f)
   ((symbol-ci<? (car tail-a) (car tail-b)) #t) ;<— symbol smaller
   ((symbol-ci<? (car tail-b) (car tail-a)) #f) ;<— symbol greater
   (else (next (cdr tail-a) (cdr tail-b)))
  )
 )

 (next list-a list-b)
)

; Creates record key of the table (tree) of generic registration.
(define (apply-generic-make-op-key op-symbol arg-symbols-list function)
 (define (find-invalid-arg l)
  (if (null? l) l
   (if (not (symbol? (car l))) (car l)
    (find-invalid-arg (cdr l))
   )
  )
 )

 (define (valid-args? l)
  (and
   (pair? l)
   (> (length l) 0)
   (null? (find-invalid-arg l))
  )
 )

 (cond
  ((not (symbol? op-symbol))
   (error "Op of apply-generic key must be a symbol!" op-symbol)
  )

  ((not (valid-args? arg-symbols-list))
   (error "Args of apply-generic must be a list of symbols!" arg-symbols-list)
  )

  ((not (procedure? function))
   (error "Pass a function to apply-generic key!" function)
  )

  (else (list op-symbol arg-symbols-list function))
 )
)

(define (apply-generic-op-key<? a b)
 (cond
  ((symbol-ci<? (car a) (car b)) #t)
  ((symbol-ci<? (car b) (car a)) #f)
  (else (symbols-list<? (cadr a) (cadr b)))
 )
)

(define (apply-generic-op-key->str k)
 (define (next tail res)
  (if (null? tail) res
   (next (cdr tail)
    (string-append
     res " "
     (symbol->string (car tail))
    )
   )
  )
 )

 (string-append "(" (symbol->string (car k)) (next (cadr k) "") ")")
)

; Function lookup table for apply-generic function.
(define ApplyGenericTable (make-tree apply-generic-op-key<?))
(define apply-generic-table '())

(define (apply-generic-put-key key)
 (define table->list (tree-op->list ApplyGenericTable))
 (define table<-list (tree-op<-list ApplyGenericTable))
 (define table-search (tree-op-search ApplyGenericTable))

 (define (put key)
  (let* (
    (old (table->list apply-generic-table))
    (new (table<-list (cons key old)))
   )
   (set! apply-generic-table new)
  )
 )

 (if (null? (table-search apply-generic-table key))
  (put key)
  (error "Key is already registered for apply-generic!"
   (apply-generic-op-key->str key))
 )
)

; Adds function to the generic apply table.
(define (apply-generic-put op-symbol arg-symbols-list function)
 (apply-generic-put-key (apply-generic-make-op-key
  op-symbol arg-symbols-list function))
)

; Registers several functions in single call.
; Each triple of arguments are for put.
(define (apply-generic-put-all . op-args-func)
 (define (make-triple l)
  (list (car l) (cadr l) (caddr l))
 )

 (define (split-triples tail res)
  (if (null? tail) res
   (if (< (length tail) 3)
    (error "No enough triples for apply-generic-put-all!")
    (split-triples (cdddr tail) (cons (make-triple tail) res))
   )
  )
 )

 (define (make-key tr)
  (apply-generic-make-op-key (car tr) (cadr tr) (caddr tr))
 )

 (let* (
   (triples (split-triples op-args-func '()))
   (keys (map make-key (reverse triples)))
  )
  (for-each apply-generic-put-key keys)
 )
)

; Finds a function registered by op symbol and
; the arguments symbols list. Returns '() on not found.
(define (apply-generic-get op-symbol arg-symbols-list)
 (define table-search (tree-op-search ApplyGenericTable))

 (let* ( ; Search by the key with irrelevant function
   (key (apply-generic-make-op-key op-symbol arg-symbols-list *))
   (res (table-search apply-generic-table key))
  )
  (if (null? res) res (caddr res))
 )
)

; General version of apply from §2.4.3
(define (apply-generic op-symbol . args)
 (let* (
   (arg-symbols-list (map apply-generic-tag-get args))
   (function (apply-generic-get op-symbol arg-symbols-list))
  )
  (if (procedure? function)
   (apply function (map apply-generic-obj args))
   (error "Apply generic function is not found!" op-symbol args)
  )
 )
)
