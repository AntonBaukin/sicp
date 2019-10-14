(define (log . args) (for-each display args) (newline))

(define (count-pairs x)
 (if (not (pair? x)) 0
  (+
   1
   (count-pairs (car x))
   (count-pairs (cdr x))
  )
 )
)

; For unique pairs it works great:
(define u '((a . (b . c)) (b . c)))

; +1 for u
; +1 for (a . (b . c))
; +1 for (b . c)
; +1 for trailing ((b . c) —>)
; +1 for second (b . c)
(log "count " u " = " (count-pairs u))

; As asked, 3 pairs is a plain list of three:
; +1 for w
; +1 for '(b c)
(define w '(a b c))
(log "count " w " = " (count-pairs w))

; Not altering the number of pairs, we just
; replace «a» with pointer to pair (c . ())
; that gives us asked 4 pairs:
(define v '(a b c))
(set-car! v (cddr v))
(log "count " v " = " (count-pairs v))


; To get 7 pairs we replace «a» with
; pointer to pair (b . –>) where we
; replace «b» with pointer to (c . ()):
(define y '(a b c))
(set-car! y (cdr y))
(set-car! (cdr y) (cddr y))
(log "count " y " = " (count-pairs y))

; Every loop, i.e. back pair pointer,
; gives us infinite recursion...
