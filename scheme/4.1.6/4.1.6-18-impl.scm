(include "filter.scm")

(define make-procedure-redefine
 (
  (lambda () ;<— immediately invoked function
   (define (transform body)
    (define defs (filter body definition?))
    (if (null? defs) body
     (list (body->let defs (filter-not body definition?)))
    )
   )

   ; What we define: a variable, or a function?
   (define (def-proc? def)
    (not (symbol? (cadr def)))
   )

   (define (def->var def)
    (if (def-proc? def) (caadr def) (cadr def))
   )

   (define (def->unassigned def)
    (list
     (def->var def)

     ; Yes, two quotes are here! Single quote means
     ; referring variable with name «*unassigned*».
     ; But we need quoted symbol instead:
     ''*unassigned*
    )
   )

   (define (def->value def)
    (if (def-proc? def)
     (def->lambda def)
     (caddr def)
    )
   )

   (define (def->lambda def)
    (append
     (list 'lambda (cdadr def))
     (cddr def)
    )
   )

   (define (body->let defs body)
    (list 'let (map def->unassigned defs) (let->let defs body))
   )

   (define (let->let defs body)
    (append (list 'let (defs->let defs)) (let->let->body defs body))
   )

   (define (rename var)
    (string->symbol (string-append "$" (symbol->string var)))
   )

   (define (defs->let defs)
    (map (lambda (d) (list (rename (def->var d)) (def->value d))) defs)
   )

   (define (def-renamed->set def)
    (list 'set! (def->var def) (rename (def->var def)))
   )

   (define (let->let->body defs body)
    (append
     (map def-renamed->set defs)
     body
    )
   )

   (define (make-proc parameters body env)
    (define body-transformed (transform body))

    (if-debug
     (log body " >>>")
     (log (car body-transformed))
    )

    (list 'procedure parameters body-transformed env)
   )

   (set! make-procedure make-proc)
  )
 )
)
