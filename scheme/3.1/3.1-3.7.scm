(include "3.1-3.3-account.scm")

(define (log . args) (for-each display args) (newline))

(define (make-joint account acc-password new-password)

 ; Check the balance as password test.
 (account acc-password)

 (lambda (pswd . cmd)
  (if (equal? pswd new-password)
   (apply account (cons acc-password cmd))
   (error "Wrong joint password!")
  )
 )
)


(define acc (make-account 100 'secret))

(log "Initial balance is: " ((acc 'secret)))
(log "Withdraw 40, left: " ((acc 'secret 'withdraw) 40))


(define joint (make-joint acc 'secret 'safe))

(log "Joint balance is: " ((joint 'safe)))
(log "Joint deposit 10, now: " ((joint 'safe 'deposit) 10))

(log "Try withdraw 1000 with wrong joint password...")
((joint 'wrong 'withdraw) 1000)
