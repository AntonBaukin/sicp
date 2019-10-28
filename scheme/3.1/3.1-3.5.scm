(include "../2.3.3/curry.scm")
(include "../2.3.3/quick-sort.scm")
(include "random.scm")
(include "enumerate-range.scm")

(define (log . args) (for-each display args) (newline))

(define (monte-carlo trials test)
 (define (monte-carlo-iter i passed)
  (if (> i trials)
   (/ passed trials) ;<— rational
   (if (test)
    (monte-carlo-iter (+ i 1) (+ passed 1))
    (monte-carlo-iter (+ i 1) passed)
   )
  )
 )

 (monte-carlo-iter 1 0)
)

(define (estimate-pi-cesaro trials rand)
 (define (cesaro-test)
  (= 1 (gcd (truncate (rand)) (truncate (rand))))
 )

 (sqrt (/ 6 (monte-carlo trials cesaro-test)))
)


(define pi 3.14159265358979323846)

(define (pi-cesaro-for-bits trials bits)
 (cons bits
  (estimate-pi-cesaro
   trials
   (make-random-n-bits 1 bits)
  )
 )
)

;(log bits "-bits random cesaro test π error = "
;  (abs (- pi pi~))
; )

; The following test shows that our generator is best
; for mod 16-bit integers with smaller number of trials.
;
; With 100 trials we have:
; 16-bits random cesaro test π error = .005342412653893014
; 11-bits random cesaro test π error = .020685006578586407
; 10-bits random cesaro test π error = .03073756939851746
;
; With 1000 trials we have:
; 18-bits random cesaro test π error = .0027685507180708058
; 14-bits random cesaro test π error = .004991224047969922
; 15-bits random cesaro test π error = .007590632899074912
; 16-bits random cesaro test π error = .007909953191461927
; 17-bits random cesaro test π error = .015574904010619672
;
; Interesting to say that with 1000 trials 16-bits random
; performs worser than with 100 ones — strange?..
;
(for-each
 (lambda (bipi)
  (log
   (car bipi) "-bits random cesaro test π error = "
   (abs (- (cdr bipi) pi))
  )
 )

 ; Sort approximations by the error ascending:
 (quick-sort
  (lambda (a b)
   (< (abs (- (cdr a) pi)) (abs (- (cdr b) pi)))
  )

  (map
   ; 1000 is the number of trials:
   (curry pi-cesaro-for-bits 1000)
   (enumerate-range 10 22)
  )
 )
)


(define (monte-carlo-integrate trials rand x0 y0 x1 y1 p)
 (define rand-x (make-random-in-range rand x0 x1))
 (define rand-y (make-random-in-range rand y0 y1))

 (*
  (monte-carlo trials (lambda () (p (rand-x) (rand-y))))
  (* (abs (- x0 x1)) (abs (- y0 y1)))
 )
)

(define (estimate-pi-circle trials)
 (monte-carlo-integrate
  trials (make-random 1) -1.0 -1.0 1.0 1.0
  (lambda (x y) (<= (+ (square x) (square y)) 1))
 )
)

(define pi-circle~ (estimate-pi-circle 100000))

; Gives: π = 3.14532 error is .0037273464102067777
(log "1-circle π estimate for 100000 trials = "
 pi-circle~ " error is " (abs (- pi-circle~ pi))
)
