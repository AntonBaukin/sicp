
(define (timestamp)
 (time->seconds (current-time))
)

(define started-at (timestamp))

; Returns delta to start-at time rounded to '.000'.
(define (ts)
 (define t (- (timestamp) started-at))
 (define x (exact (round t)))
 (define y (exact (round (* (- t x) 1000))))

 (string-append
  (number->string x)
  (cond ((< y 10) ".00") ((< y 100) ".0") (else "."))
  (number->string y)
 )
)
