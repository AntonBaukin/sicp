
(define (debug-native-list->value env val)
 (define value (resolve-value val))
 (define pair? (lookup-variable 'pair? env))
 (define exp? (list 'pair? 'value))

 (if (apply-basic-compound pair? (list value) env exp?)
  (apply-basic-compound
   (lookup-variable 'pair->log env)
   (list 10 value)
   env
   (list 'pair->log 10 'value)
  )
  value
 )
)

; Support thunks when printing debug values and lists.
(define debug-log-describe-var-value-lazy-lists
 (
  (lambda () ;<— immediately invoked function
   (define (var-value env value)
    (debug-log-describe-var-value-impl
     env
     (debug-native-list->value env value)
    )
   )

   (set! debug-log-describe-var-value var-value)
   var-value ;<— resulting function
  )
 )
)

(define debug-log-eval-msg-value-lazy-lists
 (
  (lambda () ;<— immediately invoked function
   (define (eval-msg-value exp env)
    (debug-native-list->value env (eval-impl exp env))
   )

   (set! debug-log-eval-msg-value eval-msg-value)
   eval-msg-value ;<— resulting function
  )
 )
)
