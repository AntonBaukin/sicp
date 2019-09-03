(include "./index-tree.scm")


; Returns pair function for the split: group. It takes
; a list of variables priority, and resulting list of
; split entries. Then it groups the entries and forms
; sparse polynomial.
;
; Giving '(x y z ...) variables priority, groups makes
; the terms for 'x variable with coefficients being a
; scalar, or nested polynomial for 'y variable, and
; so on recursively.
;
(define (install-polynomial-group scope)

 (define (omap-add omap i entry)
  ((cadr omap) i ;<— put to map by i-key
   ; Add entry to current list, '() on absent:
   (cons entry ((car omap) i))
  )
 )

 (define (entry-order-iter var vos)
  (if (null? vos) 0 ;<— scalar, or only else vars
   (if (eq? var (caar vos))
    (cdar vos) ;<— order of the top (var . order)
    (entry-order-iter var (cdr vos))
   )
  )
 )

 (define (entry-order var entry)
  (entry-order-iter var (cddr entry))
 )

 (define (entry-vos-without var vos res)
  (if (null? vos) res
   (if (eq? var (caar vos))
    (append res (cdr vos))
    (entry-vos-without var (cdr vos) (cons (car vos) res))
   )
  )
 )

 (define (entry-without var entry)
  (let ((vos (entry-vos-without var (cddr entry) '())))
   (if (null? vos)
    (list '* (cadr entry))
    (list '* (cadr entry) vos)
   )
  )
 )

 (define (index-entry-iter omap var entries)
  (if (null? entries) omap
   (begin
    (omap-add omap ;<— add entry to the order mapping:
     (entry-order var (car entries))
     (entry-without var (car entries))
    )

    (index-entry-iter omap var (cdr entries))
   )
  )
 )

 (define (group vars entries)
  (index-entry-iter (make-index-tree) (car vars) entries)
 )

 group ;<— resulting group function
)