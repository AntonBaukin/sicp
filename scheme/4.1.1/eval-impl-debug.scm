; Default implementation of the logging utility.
(define (debug-log . args) (for-each display args) (newline))

; Escapes the given value not to evaluate it with logging message.
(define (debug-escape value)
 (list 'debug 'escape value)
)

(define (debug-escaped? x)
 (and
  (list? x)
  (= 3 (length x))
  (eq? 'debug (car x))
  (eq? 'escape (cadr x))
 )
)

(define (debug-unescape x)
 (caddr x)
)

(define (debug-log-eval-msg-value exp env)
 (eval-impl exp env)
)

; Extended logger that treats each item as an expression
; to evaluate, thus it treats each symbol as a variable.
(define (debug-log-eval env . args)
 (apply debug-log
  (map
   (lambda (exp)
    (if (debug-escaped? exp)
     (debug-unescape exp)
     (debug-log-eval-msg-value exp env)
    )
   )
   args
  )
 )
)

; Entry point of debug commands evaluation.
(define (debug-eval-cmd env cmd . args)
 (cond
  ((eq? cmd 'log)
   (if debug-mode?
    (apply debug-log-eval (cons env args))
   )
  )
  
  ((eq? cmd 'log-env)
   (if debug-mode?
    (apply debug-log-env (append (list #t env) args))
   )
  )

  ((eq? cmd 'log-stack)
   (if debug-mode?
    (apply debug-log-stack (cons env args))
   )
  )

  ((eq? cmd 'pause)
   (if debug-mode?
    (begin
     (debug-log "*** Paused. Press Enter...")
     (read-char)
    )
   )
  )

  ((eq? cmd 'on)
   (debug-set #t)
  )

  ((eq? cmd 'off)
   (debug-set #f)
  )

  ((eq? cmd 'get)
   (debug-var-get (car args))
  )

  ((eq? cmd 'del)
   (debug-var-set (car args) void)
  )

  ((eq? cmd 'inc)
   (debug-var-inc (car args))
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

(define (debug-log-print-env-frame env frame index size)
 (debug-log "~> Frame [" index " of " size "]")

 (eval-env-frame-iterate
  frame
  (lambda (name value)
   (debug-log-print-env-frame-var env name value)
   void
  )
 )
)

(define (debug-log-print-env-frame-var env name value)
 (apply debug-log
  (append
   (list "   " name " .... ")
   (debug-log-describe-var-value env value)
  )
 )
)

(define (debug-log-describe-var-value-impl env value)
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

(define debug-log-describe-var-value
 debug-log-describe-var-value-impl
)

; Table that stores user-defined debugging variables.
(define eval-debug-vars (eval-env-frame-make))

(define (debug-var-get var)
 (eval-env-frame-lookup eval-debug-vars var)
)

(define (debug-var-set var value)
 (if (eq? void value)
  (eval-env-frame-remove eval-debug-vars var)
  (eval-env-frame-add
   eval-debug-vars
   value ;<— first comes the table value
   var   ;<- then the key, form symbol
  )
 )
)

(define (debug-var-inc var)
 (define i (debug-var-get var))
 (debug-var-set var
  (if (eq? void i) 1 (+ i 1))
 )
)
