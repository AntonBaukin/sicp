(include "../2.3.3/sorted-set.scm")
(include "void.scm")
(include "combinations.scm")
(include "bank-account.scm")

(define Numbers (make-sorted-set <))
(define nums-add (set-op-add Numbers))

(define (log . args) (for-each display args) (newline))
(define (logno . args) void)

; Shared account to withdraw:
(define $A (make-account "$A" log 0))

; Accounts to deposit:
(define $B (make-account "$B" log 0))
(define $C (make-account "$C" log 0))

(define (reset)
 (($A 'assign) 100)
 (($B 'assign) 0)
 (($C 'assign) 0)
)

(define totals-disaster '())

; In this implementation of transfer we do not synchronize
; account access, thus we get all the results possible!
(define (transfer-disaster from to amount)
 (define take-ops ((from 'withdraw) amount))
 (define give-ops ((to 'deposit) amount))
 (append (ops-join take-ops break-fail-op) give-ops)
)

(define (done-disaster)
 (define total (+ (($A 'balance)) (($B 'balance)) (($C 'balance))))
 (set! totals-disaster (nums-add totals-disaster total))
)

;(test-shuffles
; reset
; done-disaster
; (list
;  ; Transfer 75 + 50 from 100 account in parallel:
;  (transfer-disaster $A $B 75)
;  (transfer-disaster $A $C 50)
; )
;)

; This is a total non-synch disaster: 25 50 100 150 175!
;(log "Results of 100 —> 75 || 50 disaster transfers: " totals-disaster)


(define totals-serial '())

(define (transfer-serial from to amount)
 (define take-ops ((serialize (from 'withdraw)) amount))
 (define give-ops ((serialize (to 'deposit)) amount))
 (append (ops-join take-ops break-fail-op) give-ops)
)

(define (done-serial)
 (define total (+ (($A 'balance)) (($B 'balance)) (($C 'balance))))

 (set! totals-serial (nums-add totals-serial total))
 (log "$A " (($A 'balance)) " $B " (($B 'balance)) " $C " (($C 'balance)))
 (log "Total " total " —————————————————————\n")
)

;(test-shuffles
; reset
; done-serial
; (list
;  (transfer-serial $A $B 75)
;  (transfer-serial $A $C 50)
; )
;)

(reset)
(for-each-op (transfer-serial $A $B 75))

; 
;(log "Results of 100 —> 75 || 50 serial transfers: " totals-serial)

