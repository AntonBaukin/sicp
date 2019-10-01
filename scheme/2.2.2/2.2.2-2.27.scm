(define (log . args) (for-each display args) (newline))

(define nil (list))

(define (reverse l)
 (define (next res tail)
  (if (null? tail) res
   (next (cons (car tail) res) (cdr tail))
  )
 )

 (next nil l)
)

(define (deep-reverse l)
 (define (next res tail)
  (if (null? tail) res
   (if (pair? (car tail))
    (next (cons (deep-reverse (car tail)) res) (cdr tail))
    (next (cons (car tail) res) (cdr tail))
   )
  )
 )

 (next nil l)
)

(define sample (list (list 1 2) (list 3 4) 5))

(log "reverse of ((1 2) (3 4) 5)  —is—  " (reverse sample))
(log "deep reverse of ((1 2) (3 4) 5)  —is—  " (deep-reverse sample))
