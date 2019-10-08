(include "3.1-3.3-account.scm")

(define (log . args) (for-each display args) (newline))

(define acc (make-account 100 'secret))

(log "Initial balance is: " ((acc 'secret)))
(log "Withdraw 40, left: " ((acc 'secret 'withdraw) 40))

(log "Try deposit 50 with wrong password...")
((acc 'wrong 'deposit) 50)
