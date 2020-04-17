;
; First, we may not define unbind form on the user level
; (using special form «register», see «4.1.2-4.a.scm»)
; as we can't access from there functions of the evaluator
; defined in «eval-impl-*.scm» files. So, we have to add
; such as file, see «4.1.3-13-env.scm», with this one.
;
; Our special form is called «unbind». It removes variable
; from current frame of the given environment: it does not
; go up by the enclosing, or else frames of the current.
;
; It may unbind variable only of it's own call context:
; argument, or local definition. Thus it may not affect
; contexts: either visible (ancestor environments), nor
; hidden (frames or recursive calls). Removing from else
; context is cruel side-effect that breaks everything!
;
(define env-lookup-abstracts
 (
  (lambda () ;<— immediately invoked function

   (define (unbind-form exp env)
    (unbind-variable env (cadr exp))
   )

   ; Append our form to the default forms list:
   (set! eval-disp-basic-forms
    (append
     eval-disp-basic-forms
     (list 'unbind unbind-form)
    )
   )
  )
 )
)
