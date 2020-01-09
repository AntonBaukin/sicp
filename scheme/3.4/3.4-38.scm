(include "../2.3.3/sorted-set.scm")
(include "combinations.scm")

(define (log . args) (for-each display args) (newline))

(define Numbers (make-sorted-set <))
(define nums-add (set-op-add Numbers))

(define test-a '())
(define test-b '())

; Shared balance for tests a) and b):
(define balance void)

; Private balances for test b):
(define balance-Peter void)
(define balance-Pavel void)
(define balance-Maria-div void)
(define balance-Maria-sub void)

(define (reset)
 (set! balance 100)
 (log "Initial " balance)
)

(define (done-a)
 (set! test-a (nums-add test-a balance))
 (log "Final a) " balance)
 (log "——————————————————————————————————————————————————")
)

(define (Peter)
 (log "Peter adds 10")
 (set! balance (+ balance 10))
)

(define (Pavel)
 (log "Pavel takes 20")
 (set! balance (- balance 20))
)

(define (Maria)
 (log "Maria takes half")
 (set! balance (- balance (/ balance 2)))
)



(define (Peter-read)
 (log "Peter reads shared " balance)
 (set! balance-Peter balance)
)

(define (Pavel-read)
 (log "Pavel reads shared " balance)
 (set! balance-Pavel balance)
)

(define (Maria-read-div)
 (log "Maria reads shared for division " balance)
 (set! balance-Maria-div balance)
)

(define (Maria-read-sub)
 (log "Maria reads shared for subtraction " balance)
 (set! balance-Maria-sub balance)
)

(define (Peter-add)
 (log "Peter adds 10 to " balance-Peter)
 (set! balance-Peter (+ balance-Peter 10))
)

(define (Pavel-take)
 (log "Pavel takes 20 from " balance-Pavel)
 (set! balance-Pavel (- balance-Pavel 20))
)

(define (Maria-div)
 (log "Maria takes half from " balance-Maria-div)
 (set! balance-Maria-div (/ balance-Maria-div 2))
)

(define (Maria-sub)
 (log "Maria subtracts half " balance-Maria-div " from " balance-Maria-sub)
 (set! balance-Maria-sub (- balance-Maria-sub balance-Maria-div))
)

(define (Peter-save)
 (log "Peter saves " balance-Peter)
 (set! balance balance-Peter)
)

(define (Pavel-save)
 (log "Pavel saves " balance-Pavel)
 (set! balance balance-Pavel)
)

(define (Maria-save)
 (log "Maria saves " balance-Maria-sub)
 (set! balance balance-Maria-sub)
)

(define (done-b)
 (set! test-b (nums-add test-b balance))
 (log "Final b) " balance)
 (log "——————————————————————————————————————————————————")
)


(test-permutations reset done-a (list Peter Pavel Maria))
(log "\n\n\n\n")

(test-shuffles
 reset
 done-b
 (list
  (list Peter-read Peter-add Peter-save)
  (list Pavel-read Pavel-take Pavel-save)
  (list Maria-read-div Maria-div Maria-read-sub Maria-sub Maria-save)
 )
)

; Prints: Test a) results: (35 40 45 50)
(log "Test a) results: " test-a)

; Prints: Test b) results: (25 30 35 40 45 50 55 60 70 80 90 110)
(log "Test b) results: " test-b)
