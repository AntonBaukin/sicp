
; This syntax extension of IIFE (Immediately Invoked
; Function Expression) is specific to Gambit Scheme.
; Usage: (iife body-expressions)
(define-macro (iife . body)
 `((lambda () ,@body))
)

(define-macro (defiife var . body)
 `(define ,var ((lambda () ,@body)))
)
