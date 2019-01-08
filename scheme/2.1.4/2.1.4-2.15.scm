(include "2.1.4-2.y.scm")

(define iA (interval-make-% 10 1))
(define iB (interval-make-% 20 5))

(log "A = " (interval-str-% iA))
(log "B = " (interval-str-% iB))

(log "A * B ÷ (A + B) = " (interval-str-%
 (interval-div
  (interval-mul iA iB)
  (interval-add iA iB)
 )
))

(log "1 ÷ (1÷A + 1÷B) = " (interval-str-%
 (interval-inverse
  (interval-add
   (interval-inverse iA)
   (interval-inverse iB)
  )
 )
))

(log "10 * 20 ÷ (10 + 20) = " (/ 20.0 3.0))
