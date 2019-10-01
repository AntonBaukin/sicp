(include "2.1.4-2.y.scm")

(define iA (interval-make-% 5 1))
(define iB (interval-make-% 2 5))

(log "A = " (interval-str-% iA))
(log "B = " (interval-str-% iB))

(log "A * B ÷ A ≡ B = " (interval-str-%
 (interval-div (interval-mul iA iB) iA)
))

(log "A + B - A ≡ B = " (interval-str-%
 (interval-sub (interval-add iA iB) iA)
))

(log "A - B - A + B ≡ 0 = " (interval-str
 (interval-add (interval-sub (interval-sub iA iB) iA) iB)
))