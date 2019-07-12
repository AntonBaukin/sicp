; For the given positive fractional decimal in (0, 1)
; interval finds the best rational number approximating
; it using Farey sequence algorithm, where maxd — is the
; maximum denominator size. Returns (n, d) pair.
(define (make-farey-rat maxd)
 (define (close? v r)
  (< (abs (- v r)) (/ 0.05 maxd))
 )

 (define (end? b d)
  (or (> b maxd) (> d maxd))
 )

 (define (result-end a b c d)
  (if (> b maxd) (cons c d) (cons a b))
 )

 (define (result-close a b c d)
  (cond
   ((<= (+ b d) maxd)
    (cons (+ a c) (+ b d))
   )
   ((> d b) (cons (c d)))
   (else (cons (a b)))
  )
 )

 (define (median a b c d)
  (/ (* 1.0 (+ a c)) (+ b d))
 )

 (define (next r a b c d)
  (if (end? b d) (result-end a b c d)
   (let ((m (median a b c d)))
    ; (log "a = " a " b = " b " c = " c " d = " d " m = " m)
    (cond
     ((close? m r) (result-close a b c d))
     ((> r m) (next r (+ a c) (+ b d) c d))
     (else (next r a b (+ a c) (+ b d)))
    )
   )
  )
 )

 (lambda (r) (next r 0 1 1 1))
)

; Creates reducing function that converts arbitrary
; real number to an integer or rational pair.
(define (make-farey-drop-rat maxd)
 (define mine (/ 0.05 maxd))
 (define (zero? n) (< (abs n) mine))
 (define farey-rat (make-farey-rat maxd))

 (define (drop-number n)
  (let* (
    (s (if (< n 0) -1 1))    ;<— sign
    (v (abs n))              ;<— absolute value
    (i (exact (truncate v))) ;<— integer part
    (r (- v i))              ;<— fractional part
   )
   (if (zero? r)
    (* s i)
    ; Now approximate with rational fraction:
    (let ((nd (farey-rat r)))
     (if (= 1 (cdr nd))
      (* s (+ i (car nd)))
      (cons
       (* s (+ (* i (cdr nd)) (car nd)))
       (cdr nd)
      )
     )
    )
   )
  )
 )

 drop-number ;<— resulting function
)