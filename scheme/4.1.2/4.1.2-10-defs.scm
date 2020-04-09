;
; We may not fully alter the Lisp syntax as out evaluator
; has no own parser of the source texts.
;
; So, expression without parentheses are not possible:
; «if (a < b) a b» — we have to embrace it as:
; «(if (a < b) a b)» — it's not a bad.
;
; In this task we allow infix placement of binary expressions,
; such as: (a < b), (i + 1). But Lisp' variants are still
; available — we take some precautions them to work.
;
; We also allow special form of assignment: (i := <expression>),
; where «:=» is not a primary function name. We may not use
; «=» as it's numbers equality test function.
;
; Here are the rules:
;
;  1) infix expression is a list of 3 items (binary operation);
;
;  2) middle item is a symbol that resolves to a variable being
;     a procedure (primary or compound), or it's symbol «=»;
;
;  3) if the left item is a symbol, it must not be a variable
;     resolved to a procedure in the current environment.
;
; If 2) or 3) failes, — Lisp variant is the fallback.
; Yes, this tends to context-dependent runtime errors,
; but we are not too serious here...
;
(define eval-disp-else-2 ; <— phony definition
 (
  (lambda () ;<— immediately invoked function
   (define (proc? var env)
    (define value (lookup-variable-optional var env))

    (and
     (not (eq? void value))
     (or
      (procedure? value)
      (compound-procedure? value)
     )
    )
   )

   (define (assign? exp)
    (eq? ':= (cadr exp))
   )

   (define (infix? exp env)
    (and
     ; Test rule 1):
     (= 3 (length exp))
     ; Test rule 2):
     (symbol? (cadr exp))
     (or
      (and
       (symbol? (car exp))
       (assign? exp)
      )
      (proc? (cadr exp) env)
     )
     ; Test rule 3):
     (or
      (not (symbol? (car exp)))
      (not (proc? (car exp) env))
     )
    )
   )

   ; Assigns or applies reordered expression:
   (define (eval-infix exp env)
    (if (assign? exp)
     (assign-variable
      env
      (car exp)
      (eval-impl (caddr exp) env)
     )
     (eval-disp-apply
      (list (cadr exp) (car exp) (caddr exp))
      env
     )
    )
   )

   ; Use phony definition with this side-effect:
   (set! eval-disp-else
    (lambda (exp env)
     (if (infix? exp env)
      (eval-infix exp env)
      (eval-disp-apply exp env)
     )
    )
   )
  )
 )
)
