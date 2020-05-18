;
; Special form «letrec» differs from «let» implemented
; in neighbour file «eval-impl-forms.scm» in what env
; a value is evaluated: «let» — in one that includes
; it (calling env), «letrec» — in own one created
; to evaluate it's body.
;
; In this file we implement «letrec» directly,
; not as a transformation asked in task 4.20.
;
(define eval-disp-form-letrec
 (
  (lambda () ;<— immediately invoked function
   ; This inner definition we use instead of *unassigned*:
   (define (unassigned) void)
   (define (make-unassigned x) unassigned)

   (define (letrec-form exp env)
    ; The list of unassigned variables created for «letrec»:
    (define vars (map car (cadr exp)))
    (define vals (map make-unassigned vars))

    ; First, we create environment:
    (define env-ext (extend-environment vars vals env #f))

    ; Provide some info on the new environment:
    (if-debug (env-info-add env-ext 'letrec vars))

    ; Then we evaluate the values in nested environment:
    (for-each
     (lambda (exp)
      (assign-variable
       env-ext ;<— target nested environment
       (car exp)
       ; Evaluate in target (nested) environment:
       (eval-impl (cadr exp) env-ext)
      )
     )
     (cadr exp)
    )

    ; Now we evaluate the body in the extended env:
    (eval-impl (eval-wrap-begin (cddr exp)) env-ext)
   )

   (eval-disp-register-form 'letrec letrec-form)
   letrec-form ;<— resulting form
  )
 )
)
