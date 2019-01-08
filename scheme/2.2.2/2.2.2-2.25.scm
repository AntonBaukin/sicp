(define (log . args) (for-each display args) (newline))

(define a (list 1 3 (list 5 7) 9))
(define b (list (list 7)))
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(define (cadaddr l)
 (car (cdaddr l))
)

(define (cadadadadadadr l)
 (cadadr (cadadr (cadadr l)))
)

(log "a = " a "; (cadaddr a) = " (cadaddr a))
(log "b = " b "; (caar b) = " (caar b))
(log "c = " c "; (cadadadadadadr c) = " (cadadadadadadr c))
