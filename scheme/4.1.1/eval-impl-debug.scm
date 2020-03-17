; Default implementation of the logging utility.
(define (debug-log . args) (for-each display args) (newline))

; Entry point of debug commands evaluation.
(define (debug-eval-cmd env cmd . args)
 (cond
  ((eq? cmd 'log)
   (apply debug-log args)
  )
  ((eq? cmd 'log-env)
   (apply debug-log-env (append (list #t env) args))
  )
  ((eq? cmd 'log-stack)
   (apply debug-log-stack (cons env args))
  )

  (else (error "Unknown debug command" cmd))
 )
)

(define (debug-log-env without-global? env . msgs)
 (define (stop? env)
  (or (null? env)
   (and
    without-global?
    (null? (enclosing-environment env))
   )
  )
 )

 (define (next env)
  (if (not (stop? env))
   (begin
    (debug-log-print-env env)
    (debug-log-print-env-frames env)
    (next (enclosing-environment env))
   )
  )
 )

 (if (not (null? msgs))
  (apply debug-log msgs)
 )

 (next env)
)

(define (debug-log-stack env . msgs)
 (if (not (null? msgs))
  (apply debug-log msgs)
 )

 
 
)

(define (debug-log-print-env env)
 (define (info-data info res)
  (cond
   ((null? info) res)

   ((procedure? (car info))
    (info-data (cdr info) res)
   )

   (else
    (info-data
     (cdr info)
     (debug-log-print-env-add-info-data res (car info))
    )
   )
  )
 )

 (apply debug-log
  (debug-log-print-env-fmt env
   (reverse (info-data (eval-env-info env) '()))
  )
 )
)

(define (debug-log-print-env-fmt env info-data)
 (define (ins-space tail res)
  (cond
   ((null? tail) res)

   ((null? res)
    (ins-space (cdr tail) (cons (car tail) '()))
   )

   (else
    (ins-space (cdr tail)
     (cons (car tail) (cons " " res))
    )
   )
  )
 )

 (cons "\n> Env #" (reverse (ins-space info-data '())))
)

(define (debug-log-print-env-add-info-data res info-item)
 (cond
  ((compound-procedure? info-item)
   (cons (procedure-parameters info-item) res)
  )

  (else (cons info-item res))
 )

)

(define (debug-log-print-env-frames env)
 (for-each-frame env debug-log-print-env-frame)
)

(define (debug-log-print-env-frame frame index size)
 (debug-log "~> Frame [" index " of " size "]")

 ((table-op-iterate EvalEnvFrame)
  frame
  (lambda (name value)
   (debug-log-print-env-frame-var name value)
   void
  )
 )
)

(define (debug-log-print-env-frame-var name value)
 (debug-log "   " name " .... " (debug-log-describe-var-value value))
)

(define (debug-log-describe-var-value value)
 (cond
  ((procedure? value) "#<procedure>")

  ((compound-procedure? value)
   (apply string-append
    (append
     '("#<compound-procedure ( ")
     (map
      (lambda (p)
       (string-append (symbol->string p) " ")
      )
      (procedure-parameters value)
     )
     '(")>")
    )
   )
  )

  (else value)
 )
)
