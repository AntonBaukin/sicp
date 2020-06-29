;
; Native functions c...r (caar, cdar, caddr, ...) must be
; defined on upper level in the case of lazy evaluator,
; as they go deeply into paired structures without resolving
; our thunks. They are now a part of our standard library.
;
(eval-basic
 (define (caar p)
  (car (car p))
 )

 (define (cdar p)
  (cdr (car p))
 )

 (define (cadr p)
  (car (cdr p))
 )

 (define (cddr p)
  (cdr (cdr p))
 )

 (define (caddr p)
  (car (cdr (cdr p)))
 )

 (define (cdddr p)
  (cdr (cdr (cdr p)))
 )


 ; Define global functions:
 (global caar    caar)
 (global cdar    cdar)
 (global cadr    cadr)
 (global cddr    cddr)
 (global caddr   caddr)
 (global cdddr   cdddr)
)
