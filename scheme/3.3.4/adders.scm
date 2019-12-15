
; Creates half-adder of two bits.
; Half adder time = max(OR, AND + NOT) + AND.
(define (half-adder in0 in1 sum carry)
 (define d (make-wire sum))
 (define e (make-wire sum))
 (define o (or-gate in0 in1 d))
 (define a (and-gate in0 in1 carry))
 (define i (inverter carry e))
 (define s (and-gate d e sum))

 (list 'half-adder (get-agenda sum) d e o a i s)
)

; Creates full-adder of three bits.
; Full adder max time is: 2 x Half-Adder.
(define (full-adder in0 in1 carry-in sum carry-out)
 (define s (make-wire sum))
 (define c0 (make-wire carry-out))
 (define c1 (make-wire carry-out))
 (define a0 (half-adder in1 carry-in s c0))
 (define a1 (half-adder in0 s sum c1))
 (define o (or-gate c0 c1 carry-out))

 (list 'full-adder (get-agenda sum) s c0 c1 a0 a1 o)
)
