
; Function that removes keys from our frame (tree table).
(define eval-env-frame-remove (table-op-remove EvalEnvFrame))

; See remarks from «4.1.3-13-forms.scm».
(define (unbind-variable env name)
 (eval-env-frame-remove (first-frame env) name)
)
