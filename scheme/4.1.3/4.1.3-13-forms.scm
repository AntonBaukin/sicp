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
(define eval-disp-form-unbind
 (
  (lambda () ;<— immediately invoked function

   ; Function that removes keys from our frame (tree table).
   (define eval-env-frame-remove (table-op-remove EvalEnvFrame))

   ; See remarks from «4.1.3-13-forms.scm».
   (define (unbind-variable env name)
    (eval-env-frame-remove (first-frame env) name)
   )

   (define (unbind-form exp env)
    (unbind-variable env (cadr exp))
   )
   
   (eval-disp-register-form 'unbind unbind-form)
   unbind-form ;<— resulting form
  )
 )
)
