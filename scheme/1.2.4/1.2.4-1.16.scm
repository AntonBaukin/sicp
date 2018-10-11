(define (log . args) (for-each display args) (newline))

(define (pow-plain v n) (if (= n 0) 1 (* v (pow-plain v (- n 1)))))

(define (pow-fastr v n) (cond
  ((= n 0) 1)
  ((even? n) (square (pow-fastr v (/ n 2))))
  (else (* v (pow-fastr v (- n 1))))
))

(define (pow-fasti v n)
 ; i — the pow left, r — accumulated result, vx — current v ^ x
 (define (step i r x vx)
  ;(log "i = " i ", r = " r ", x = " x ", v^x = " vx)
  (cond
   ((= i 0) r)
   ((= i 1) (* r v))

   ; {can use current pow}
   ((>= i x) (step (- i x) (* r vx) (* x 2) (square vx)))

   ; start again
   (else (step i r 2 (square v)))
  )
 )

 (step n 1 2 (square v))
)


(log "plain 2 ^ 8 = " (pow-plain 2 8))
(log "fastr 2 ^ 8 = " (pow-fastr 2 8))
(log "fasti 2 ^ 8 = " (pow-fasti 2 8))
(newline)
(log "plain 3 ^ 5 = " (pow-plain 3 5))
(log "fastr 3 ^ 5 = " (pow-fastr 3 5))
(log "fasti 3 ^ 5 = " (pow-fasti 3 5))
(newline)
(log "plain 2 ^ 35 = " (pow-plain 2 35))
(log "fastr 2 ^ 35 = " (pow-fastr 2 35))
(log "fasti 2 ^ 35 = " (pow-fasti 2 35))
