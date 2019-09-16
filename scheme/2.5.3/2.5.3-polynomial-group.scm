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
(define (install-polynomial-group polynomial-package split-package scope)
 (define make-poly (car polynomial-package))
 (define merge-entries (cadr split-package))

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
    (append (list '* (cadr entry)) vos)
   )
  )
 )

 (define (index-entry-iter omap var entries)
  (if (null? entries) omap
   (begin
    (omap-add omap ;<— add entry to the order mapping
     (entry-order var (car entries))
     (entry-without var (car entries))
    )

    (index-entry-iter omap var (cdr entries))
   )
  )
 )

 ; Scalar entry has single variable with 0 order.
 (define (scalar-entry? e) (= 2 (length e)))
 (define (scalar-entry-coeff e) (cadr e))
 (define (single-scalar-entry? entries)
  (and (null? (cdr entries)) (scalar-entry? (car entries)))
 )

 ; Creates sparse term coefficient from an entry.
 (define (make-term vars order entries)
  (cons order
   (if (single-scalar-entry? entries)
    (scalar-entry-coeff (car entries))
    (group vars entries)
   )
  )
 )

 (define (make-terms vars omap)
  (define terms '())

  ((index-tree-iter omap)
   (lambda (order entry)
    (set! terms (cons (make-term vars order entry) terms))
    void
   )
  )

  terms
 )

 (define (group vars entries)
  (make-poly (car vars)
   (make-terms (cdr vars)
    (index-entry-iter (make-index-tree) (car vars) entries)
   )
  )
 )

 group ;<— resulting group function
)