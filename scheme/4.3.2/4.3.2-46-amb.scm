;
; Here we alter «amb» call sequence in a simple way:
; we reverse the order of the arguments evaluation.
;
(define amb-apply-args-alt
 (
  (lambda () ;<— immediately invoked function
   (define apply-args-std amb-apply-args)

   (define (apply-args-alt success fail aps env)
    (apply-args-std ;<— call standard version
     (lambda (fail2 args)
      ; Reverse back the arguments:
      (success fail2 (reverse args))
     )
     fail
     ; Reverse the argument executors:
     ;(reverse aps)
     (reverse aps)
     env
    )
   )

   (set! amb-apply-args apply-args-alt)
   apply-args-alt
  )
 )
)
