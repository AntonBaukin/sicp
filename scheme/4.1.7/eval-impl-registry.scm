;
; In the following table we register analyzing evaluators
; of special forms. Each analyzer take «exp» and returns
; function that takes «env».
;
; For this table we reuse class ops EvalEnvFrame from
; the environment — variable «eval-analyze-table».
;
(define eval-analyze-table (eval-env-frame-make))

; Collection of basic forms to register.
(define eval-disp-basic-forms '())

; Register takes arguments (form-symbol form-proc ...)
; and adds the form processors to the table.
(define (eval-disp-register . forms)
 (define (next tail)
  (eval-env-frame-add
   eval-analyze-table
   (cadr tail) ;<— first comes the table value
   (car tail)  ;<- then the key, form symbol
  )

  (if (not (null? (cddr tail)))
   (next (cddr tail))
  )
 )

 (next forms)
)

; Appends form to the dispatcher forms list.
;
; A form works on the implementation level,
; and it has access to all included as
; «eval-impl-*.scm» routines.
;
; Unlike forms of §4.1.2 and §4.1.6, analyzing
; dispatching evaluator of §4.1.7 has all the
; forms collected via this call, and the initial
; list «eval-disp-basic-forms» is empty.
;
(define (eval-disp-register-form name form-proc)
 (set! eval-disp-basic-forms
  (append
   eval-disp-basic-forms
   (list name form-proc)
  )
 )
)

(define (eval-lookup-form name)
 (eval-env-frame-lookup eval-analyze-table name)
)
