(define (log . args) (for-each display args) (newline))

(define (append! head tail)
 (set-cdr! (last-pair head) tail)
 head
)

(define (last-pair l)
 (if (null? (cdr l)) l
  (last-pair (cdr l))
 )
)

(define x '(a b))
(log "x = " x)

(define y '(c d))
(log "y = " y)

; «z» is a new list '(a b c d)
(define z (append x y))
(log "z = (append x y) = " z)

; (cdr x) is original last pair of x = '(b)
(log "(cdr x) = " (cdr x))

; Updates (b . ()) leaving (a . —>) intact
(define w (append! x y))

(log "w = (append! x y) = " w)

; Yes, «x» and «w» are the same pair:
(log "(eq? x w) = " (eq? x w))

; Second pair of «x» is now connected to «y»:
(log "(cdr x) = " (cdr x)) ; > '(b c d)
