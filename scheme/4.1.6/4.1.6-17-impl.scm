(include "filter.scm")

(define make-procedure-redefine
 (
  (lambda () ;<— immediately invoked function
   (define (transform body)
    (define defs (filter body definition?))
    (if (null? defs) body
     (body->assign defs (filter-not body definition?))
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
     'define
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

   (define (def->set def)
    (list 'set! (def->var def) (def->value def))
   )

   (define (body->assign defs body)
    (append
     (map def->unassigned defs)
     (map def->set defs)
     body
    )
   )

   (define (make-proc parameters body env)
    (list 'procedure parameters (transform body) env)
   )

   (set! make-procedure make-proc)
  )
 )
)
