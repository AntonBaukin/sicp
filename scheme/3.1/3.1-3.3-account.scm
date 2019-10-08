
(define (make-account balance password)
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

 (define (check-pswd pswd op)
  (if (equal? pswd password)
   op
   (error "Wrong password!")
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