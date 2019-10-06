(define (log . args) (for-each display args) (newline))

(define (make-account check-tries call-cops balance password)
 (define (deposit amount)
  (if (<= amount 0)
   (error "Wrong deposit amount" amount)
   (begin
    (set! balance (+ balance amount))
    balance
   )
  )
 )

 (define (withdraw amount)
  (cond
   ((<= amount 0)
    (error "Wrong withdraw amount" amount)
   )

   ((< balance amount)
    (error "Not enough money to withdraw" balance amount)
   )

   (else
    (set! balance (- balance amount))
    balance
   )
  )
 )

 ; Resulting function on a wrong password.
 (define (wrong-password . args)
  "Wrong password!"
 )

 (define wrong-tries 0)

 (define (check-pswd pswd op)
  (if (equal? pswd password)
   (begin
    (set! wrong-tries 0) ;<— reset the counter on success
    op
   )
   (begin
    (set! wrong-tries (+ wrong-tries 1))
    (if (> wrong-tries check-tries)
     (call-cops)
     wrong-password
    )
   )
  )
 )

 (lambda (pswd . cmd)
  (cond
   ((null? cmd)
    (check-pswd pswd (lambda () balance))
   )

   ((eq? (car cmd) 'withdraw)
    (check-pswd pswd withdraw)
   )

   ((eq? (car cmd) 'deposit)
    (check-pswd pswd deposit)
   )

   (else
    (error "Unknown account operation" cmd)
   )
  )
 )
)

(define (call-cops)
 (error "Keep the change, ya filthy animal!")
)

(define acc (make-account 7 call-cops 100 'secret))

(log "Initial balance is: " ((acc 'secret)))
(log "Deposit 50, now: " ((acc 'secret 'deposit) 50))

(log "Try withdraw 1000 with wrong password:")
(log "Try 1") ((acc 'wrong-1 'withdraw) 1000)
(log "Try 2") ((acc 'wrong-2 'withdraw) 1000)
(log "Try 3") ((acc 'wrong-3 'withdraw) 1000)
(log "Try 4") ((acc 'wrong-4 'withdraw) 1000)
(log "Try 5") ((acc 'wrong-5 'withdraw) 1000)
(log "Try 6") ((acc 'wrong-6 'withdraw) 1000)
(log "Try 7") ((acc 'wrong-7 'withdraw) 1000)

(log "Try 8, something happens...")
((acc 'wrong-8 'withdraw) 1000)
