
; Produces list of integers [a .. b], where
; «a» may be smaller or greater than «b».
; Note: «b» is the last item of the result.
(define (enumerate-range a b)
 (define s (if (< a b) -1 +1))
 (define v (if (< a b) < >))

 (define (iter i res)
  (if (v i a) res (iter (+ i s) (cons i res)))
 )

 (iter b '())
)

; Produces list of integers 0 .. n-1.
(define (enumerate-n n)
 (enumerate-range 0 (- n 1))
)

; Produces list of n items with the given producer.
; Items are returned in the production order.
(define (produce-n n producer)
 (define (next i res)
  (if (>= i n) res
   (next (+ i 1) (cons (producer) res))
  )
 )

 (reverse (next 0 '()))
)

; Produces list of n *unique* items with the given
; producer. Be careful with this routine!
; Items are compared with eq? predicate.
; Production order is preserved.
(define (produce-n-unique n eq? producer)
 (define (contains? set x)
  (if (null? set) #f
   (if (eq? (car set) x) #t
    (contains? (cdr set) x)
   )
  )
 )

 (define (next i res)
  (if (>= i n) res
   (let ((x (producer)))
    (if (contains? res x)
     (next i res) ;<— try again
     (next (+ i 1) (cons x res))
    )
   )
  )
 )

 (reverse (next 0 '()))
)
