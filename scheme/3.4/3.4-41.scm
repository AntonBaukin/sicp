(include "../2.3.3/sorted-set.scm")
(include "void.scm")
(include "combinations.scm")
(include "bank-account-ops.scm")

(define Numbers (make-sorted-set <))
(define nums-add (set-op-add Numbers))

(define log? #f) ;<— flag of conditional logging

(define (log . args) (for-each display args) (newline))
(define (logx . args) (if log? (apply log args)))

; Shared account to withdraw:
(define $A (make-account "$A" logx 0))

; Accounts to deposit:
(define $B (make-account "$B" logx 0))
(define $C (make-account "$C" logx 0))

(define (reset)
 (($A 'assign) 100)
 (($B 'assign) 0)
 (($C 'assign) 0)
)

(define totals-disaster '())

; In this implementation of transfer we do not synchronize
; account access, thus we get all the results possible!
(define (transfer-disaster from to amount)
 (ops-join
  ((from 'withdraw) amount)
  ((to 'deposit) amount)
 )
)

(define (done-disaster)
 (define total (+ (($A 'balance)) (($B 'balance)) (($C 'balance))))
 (set! totals-disaster (nums-add totals-disaster total))
 (logx "Total " total " —————————————————————\n")
)

(test-shuffles
 reset
 done-disaster
 (list
  ; Transfer 75 + 50 from 100 account in parallel:
  (transfer-disaster $A $B 75)
  (transfer-disaster $A $C 50)
 )
)

; This is a total non-synch disaster: 100 150 175!
(log "Results of 100 —> 75 || 50 disaster transfers: " totals-disaster)


(define totals-serial '())

(define (transfer-serial from to amount)
 (ops-join
  ((serialize (from 'withdraw)) amount)
  ((serialize (to 'deposit)) amount)
 )
)

(define (done-serial)
 (define total (+ (($A 'balance)) (($B 'balance)) (($C 'balance))))

 (set! totals-serial (nums-add totals-serial total))
 (logx "Total " total " —————————————————————\n")
)

;(set! log? #t)
;(log "\n" "Correct variants of serialized transfers:")

(test-shuffles
 reset
 done-serial
 (list
  (transfer-serial $A $B 75)
  (transfer-serial $A $C 50)
 )
)

(log "Results of 100 —> 75 || 50 serial transfers: " totals-serial)
(set! log? #f)


(define totals '())
(define total void)

; This function demonstrates classical case for databases:
; non repeatable read — possible without snapshot isolation,
; or record read locks. When reading account balance while
; not atomic transfer, we see intermediate (yes, committed)
; results that do break global data invariant.
;
(define (calc-total)
 (list
  (lambda ()
   (define balance (($A 'balance)))
   (set! total balance)
   (logx "Total <~ $A " balance)
  )

  (lambda ()
   (define balance (($B 'balance)))
   (logx "Total " total " <~+ $B " balance)
   (set! total (+ total balance))

  )

  (lambda ()
   (define balance (($C 'balance)))
   (logx "Total " total " <~+ $C " balance)
   (set! total (+ total balance))
  )
 )
)

(define (done-total)
 (set! totals (nums-add totals total))
 (logx "Total " total " —————————————————————\n")
)

(test-shuffles
 reset
 done-total
 (list
  (transfer-serial $A $B 75)
  (transfer-serial $A $C 50)
  (calc-total)
 )
)

; Resulting totals here are also a total disaster as we
; freely read intermedaiate values: 25 50 100 150 175!
(log "Totals of non-repeatable reads while serial transfers: " totals)
(set! totals '())

(test-shuffles
 reset
 done-total
 (list
  (transfer-serial $A $B 75)
  (transfer-serial $A $C 50)
  ((serialize calc-total))
 )
)

; We still see intermediate balances as concurrent transfers are
; not serialized (mutual locks): 25 50 100. Yes, total balance
; in this case may be smaller, but not over initial 100!
(log "Totals of serialized reads while serial transfers: " totals)
