; Default implementation of the logging utility.
(define (debug-log . args) (for-each display args) (newline))

; Extended logger that treats each item as an expression
; to evaluate, thus it treats each symbol as a variable.
(define (debug-log-eval env . args)
 (apply debug-log
  (map (lambda (exp) (eval-impl exp env)) args)
 )
)

; Entry point of debug commands evaluation.
(define (debug-eval-cmd env cmd . args)
 (cond
  ((eq? cmd 'log)
   (apply debug-log-eval (cons env args))
  )
  ((eq? cmd 'log-env)
   (apply debug-log-env (append (list #t env) args))
  )
  ((eq? cmd 'log-stack)
   (apply debug-log-stack (cons env args))
  )

  ((eq? cmd 'pause)
   (debug-log "*** Paused. Press Enter...")
   (read-char)
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
  (apply debug-log-eval (cons env msgs))
 )

 (next env)
)

(define (debug-log-stack env . msgs)
 (if (not (null? msgs))
  (apply debug-log-eval (cons env msgs))
 )

 (debug-log-print-env-frames env)
)

(define (debug-log-print-env env)
 (apply debug-log (debug-log-print-env-fmt env "\n> Env #"))
)

(define (debug-log-env-info-data env)
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

 (reverse (info-data (eval-env-info env) '()))
)

(define (debug-log-print-env-fmt env prefix)
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

 (cons
  prefix
  (reverse
   (ins-space (debug-log-env-info-data env) '())
  )
 )
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
 (apply debug-log
  (append
   (list "   " name " .... ")
   (debug-log-describe-var-value value)
  )
 )
)

(define (debug-log-describe-var-value value)
 (cond
  ((procedure? value) '("#<procedure>"))

  ((compound-procedure? value)
   (list
    "#<compound-procedure "
    (procedure-parameters value)
    ">"
   )
  )

  ((environment? value)
   (debug-log-print-env-fmt value "Env #")
  )

  (else (list value))
 )
)
