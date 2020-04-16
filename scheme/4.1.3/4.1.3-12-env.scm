;
; Here we define phony variable to assign new implementations
; of basic environment routines: lookup — «lookup-variable-optional»,
; and set — «assign-variable». Define routine does not require
; generic implementation as it updates only the first frame
; of the target environment.
;
(define env-lookup-abstracts
 (
  (lambda () ;<— immediately invoked function

   ; Traces up the first frames of the given environment
   ; and invokes the routine when it finds the definition.
   ; Returns void when the routine was not invoked, else
   ; returns the value returned by it.
   ;
   ; Routine arguments: (env value).
   ;
   (define (trace-env name env routine)
    (define value
     (if (not (null? env))
      (env-frame-lookup name env)
     )
    )

    (cond
     ((null? env) void)
     ((not (eq? void value))
      (routine env value)
     )
     (else
      (trace-env name (enclosing-environment env) routine)
     )
    )
   )

   (define (own-lookup name env)
    ; Lookup just returns the found value:
    (trace-env name env (lambda (env value) value))
   )

   (define (own-assign env name value)
    (if
     (eq? void
      (trace-env name env
       ; Here we need not the value, but the environment,
       ; where it was found, then we overwrite it:
       (lambda (env old-value)
        (eval-env-frame-add (first-frame env) value name)
       )
      )
     )
     (error "Unbound variable name" name)
     value ;<— return the new value
    )
   )

   ; Assign implementations based on generic tracer:
   (set! lookup-variable-optional own-lookup)
   (set! assign-variable own-assign)
  )
 )
)
