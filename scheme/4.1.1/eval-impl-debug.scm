; Default implementation of the logging utility.
(define (debug-log . args) (for-each display args) (newline))

; Adds given to the info list of the environment.
; (Alters the info list instance.)
(define (env-info-add env . items)
 (eval-env-info-set
  env
  (reverse
   (append
    (reverse items)
    (reverse (eval-env-info env))
   )
  )
 )
)

(define (env-info-has? env item)
 (define (next info)
  (cond
   ((null? info) #f)
   ((eq? item (car info)) #t)
   (else (next (cdr info)))
  )
 )

 (next (eval-env-info env))
)

(define (env-info-miss? env item)
 (not (env-info-has? env item))
)

; Entry point of debug commands evaluation.
(define (debug-eval-cmd env cmd . args)
 (cond
  ((eq? cmd 'log-env)
   (apply debug-log-env (cons env args))
  )

  (else (error "Unknown debug command" cmd))
 )
)

(define (debug-log-env env . msgs)
 (define (next env)
  (if (not (null? env))
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

(define (debug-log-print-env env)
 (define (info-data info res)
  (cond
   ((null? info) res)
   ((procedure? (car info)) (info-data (cdr info) res))
   (else
    (info-data
     (cdr info)
     (cons " " (cons (car info) res))
    )
   )
  )
 )

 (apply debug-log
  (cons "\n> Env #"
   (reverse (info-data (eval-env-info env) '()))
  )
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
   (debug-log "   " name " .... " value)
   void
  )
 )
)
