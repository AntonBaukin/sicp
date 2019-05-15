(include "../2.3.3/tree.scm")

; Incapsulates building of ApplyGenericScope tree [type].
; This makes some functions of §2.4.3 not visible.
(define (apply-generic-make-scope-type)

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

 ; Scope is a tree with this comparator that first checks
 ; the operation symbols, then the lists of the operands.
 (define (op-key<? a b)
  (cond
   ((symbol-ci<? (car a) (car b)) #t)
   ((symbol-ci<? (car b) (car a)) #f)
   (else (symbols-list<? (cadr a) (cadr b)))
  )
 )

 ; Creates the scope tree for functions lookup.
 (make-tree op-key<?)
)

; Returns symbol tagging object wrapped in a tagged pair.
(define (apply-generic-tag-get tagged-obj)
 (if (or
   (not (pair? tagged-obj))
   (not (symbol? (car tagged-obj)))
  )
  (error "Not a tagged object: " tagged-obj)
  (car tagged-obj)
 )
)

; Returns object wrapped in a tagged pair.
(define (apply-generic-unwrap tagged-obj)
 (if (or
   (not (pair? tagged-obj))
   (not (symbol? (car tagged-obj)))
  )
  (error "Not a tagged object: " tagged-obj)
  (cdr tagged-obj)
 )
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

; Converts op-key (used for table lookup) to string.
; Used for logging and error reporting.
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

; General implementation of the register function.
(define (apply-generic-register-impl put-safe . op-args-func)
 (define (make-triple l)
  (list (car l) (cadr l) (caddr l))
 )

 (define (split-triples tail res)
  (if (null? tail) res
   (if (< (length tail) 3)
    (error "No enough triples for apply-generic register!")
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
  (for-each put-safe keys)
 )
)
