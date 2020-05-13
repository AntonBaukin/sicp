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

   (define (def->promise def)
    (list
     'define
     (def->var def)

     ; We do not wrap directly in a promise as a promise
     ; may be a defined value itself. We tag our promise
     ; with symbol 'promised.
     (list 'cons
      ''promised ;<— two quotes makes a single one
      (list 'delay (def->value def))
     )
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
     (map def->promise defs)
     body
    )
   )

   (define (make-proc parameters body env)
    (if-debug
     (log body " >>> ")
     (log (transform body))
    )

    (list 'procedure parameters (transform body) env)
   )

   (set! make-procedure make-proc)
  )
 )
)
