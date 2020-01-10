
; To emulate various effects of parallel execution,
; we split each function of account processing into
; list of atomic ops. Then we combinate these ops.
(define (make-account id log balance)
 (define (deposit amount)
  (define local void)

  (list
   (lambda ()
    (set! local balance)
    (log id " deposit read ~> " local)
   )

   (lambda ()
    (log id " deposit add " local " + " amount)
    (set! local (+ local amount))
   )

   (lambda ()
    (set! balance local)
    (log id " deposit save <~ " local)
    #t
   )
  )
 )

 (define (withdraw amount)
  (define local void)

  (list
   (lambda ()
    (set! local balance)
    (log id " withdraw read ~> " local)
   )

   (lambda ()
    (if (> local amount)
     (begin
      (log id " withdraw sub " local " - " amount)
      (set! local (- local amount))
     )
     (begin
      (set! local #f)
      (log id " withdraw insufficient for " amount)
     )
    )
   )

   (lambda ()
    (if (eq? #f local) #f
     (begin
      (set! balance local)
      (log id " withdraw save <~ " local)
      #t
     )
    )
   )
  )
 )

 ; There is no need to make balance getter a single op
 ; list as we can't emulate concurrent read from not
 ; serialized operation...
 (define (get)
  balance
 )

 ; Used only for test purposes:
 (define (set amount)
  (set! balance amount)
 )
 
 (lambda (m)
  (cond
   ((eq? m 'deposit)  deposit)
   ((eq? m 'withdraw) withdraw)
   ((eq? m 'balance)  get)
   ((eq? m 'assign)   set)
  )
 )
)

; As we split procedure into a list of «atomic» ops,
; serialize here just turns this list into single op.
; In SICP serialize is high-order function, thus here
; we also have to make function factory.
(define (serialize ops-maker)
 (define (make-single . args)
  (define ops (apply ops-maker args))

  (define (single)
   (define result void)

   (for-each
    (lambda (op) (set! result (op)))
    (apply ops-maker args)
   )

   result
  )

  (list single)
 )

 make-single
)

; Rewrites the last op of the given ops list: invokes this
; last op and gives the result to the given op.
(define (ops-join ops op)
 (define rops (reverse ops))
 (define (newop) (op ((car rops))))
 (reverse (cons newop (cdr rops)))
)

(define (break-fail-op success)
 (if (eq? #f success) 'break)
)
