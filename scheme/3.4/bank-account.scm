(include "serializer.scm")
(include "ts.scm")


; We also add busy time delays to emulate overlap of the
; threads, — else we are not able to get multithreading
; collisions of tiny critical sections.
(define (make-account id log balance ddelay wdelay)
 (define serializer (make-serializer))

 (define (logx . args)
  (define t (thread-name (current-thread)))
  (apply log (append (list (ts) " [§" t "] @" id " ") args))
 )

 (define (deposit amount)
  (define local (+ balance amount))
  (logx "deposit " amount " await " ddelay "sec…")
  (thread-sleep! ddelay)
  (set! balance local)
  (logx "deposit " amount " done")
 )

 (define (withdraw amount)
  (define local (- balance amount))
  (logx "withdraw " amount " await " wdelay "sec…")
  (thread-sleep! wdelay)
  (if (< local 0)
   (begin
    (logx "withdraw " amount " denied")
    #f
   )
   (begin
    (set! balance local)
    (logx "withdraw " amount " succeed")
    #t
   )
  )
 )

 (define (get)
  balance
 )

 ; All these ops are not internally serialized not to
 ; rewrite this file for each concreete SICP task.
 ; We write external functions instead.
 (lambda (m)
  (cond
   ((eq? m 'id)         id)
   ((eq? m 'deposit)    deposit)
   ((eq? m 'withdraw)   withdraw)
   ((eq? m 'balance)    get)
   ((eq? m 'serializer) serializer)
  )
 )
)

(define (account-serial account op-symbol . args)
 (apply ((account 'serializer) (account op-symbol)) args)
)

; Returns function that takes accounts list, locks them
; in the IDs ascending order, then invokes the given task.
(define (mutual-lock accounts locker task)
 (define sorted
  (quick-sort
   (lambda (a b) (string-ci<? (a 'id) (b 'id)))
   accounts
  )
 )

 ; We pass the origial task recursively, and on return
 ; wrap into lock in the reversed order — thus we form
 ; proper nesting of the locks.
 (define (lock-next tail task)
  (if (null? tail) task
   (locker (car tail) (lock-next (cdr tail) task))
  )
 )

 (lock-next sorted task) ;<— returns the function
)
