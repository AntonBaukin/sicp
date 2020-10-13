; Copy from «3.1/random.scm».
(define (make-random seed)
 (define n seed)
 (define a 25214903917)
 (define c 11)
 (define m 281474976710656)

 (define (random . cmd)
  (if (null? cmd) void
   (if (eq? 'reset (car cmd))
    (set! n seed)
    (set! n (car seed))
   )
  )

  (set! n (modulo (+ c (* n a)) m))
  (modulo
   (truncate-quotient n 65536) ; <— shift 16 bits
   4294967296 ; <— and with 32 bits
  )
 )

 random
)

(define (shuffle random sequence)
 (define (ith head tail i)
  (if (= 0 i)
   (cons (car tail) (append head (cdr tail)))
   (ith (cons (car tail) head) (cdr tail) (- i 1))
  )
 )

 (define (next sequence len res)
  (cond
   ((= 0 len) res)
   ((= 1 len) (cons (car sequence) res))
   (else
    (let* (
      (i (modulo (random) len))
      (x (ith '() sequence i))
     )
     (next
      (cdr x)
      (- len 1)
      (cons (car x) res)
     )
    )
   )
  )
 )

 (next sequence (length sequence) '())
)

;
; This IIF installs two new special forms: «ramb»,
; and «ramb-seed» — it assigns the random seed.
; It also replaces «amb» for random choose!
;
; Note that here «ramb» takes single list argument!
; Hence, it pre-evaluates all expressions.
;
(define eval-amb-form-ramb
 (
  (lambda () ;<— immediately invoked function
   (define random (make-random 1000003)) ;<— default seed

   ; In «set» form we do save the previous value of the variable
   ; to restore it in fail-branch. With «ramb» we do this not as
   ; we want to alter the following selections of «ramb».
   ;
   (define (ramb-seed-form exp)
    (define seedp (eval-analyze (cadr exp)))

    (lambda (success fail env) ;<— execution procedure
     (seedp
      (lambda (fail2 value)
       ; Here is the side-effect not restored on fail:
       (set! random (make-random value))
       (success fail2 value)
      )
      fail
      env
     )
    )
   )

   (define (ramb-form exp)
    (define ps (eval-analyze (cadr exp)))

    (lambda (success fail env) ;<— execution procedure
     (ps
      (lambda (fail2 choices)
       (define (try-next choices)
        (if (null? choices)
         (fail2)
         (success
          (lambda () (try-next (cdr choices)))
          (car choices)
         )
        )
       )

       (try-next (shuffle random choices))
      )
      fail
      env
     )
    )
   )

   (define (amb-form exp)
    (define ps (map eval-analyze (cdr exp)))

    (lambda (success fail env) ;<— execution procedure
     (define (try-next choices)
      (if (null? choices)
       (fail)
       ((car choices) ;<— call p-ith executor
        success
        (lambda () (try-next (cdr choices)))
        env
       )
      )
     )

     (try-next (shuffle random ps))
    )
   )

   (eval-disp-register-form 'ramb-seed ramb-seed-form)
   (eval-disp-register-form 'ramb ramb-form)
   (eval-disp-register-form 'amb amb-form)
   ramb-form ;<— resulting form
  )
 )
)
