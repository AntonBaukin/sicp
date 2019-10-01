(include "2.1.1-2.1.scm")
(define (log . args) (for-each display args) (newline))

;(log "1/3 = " (rat-str (rat-make 1 3)))
;(log "3/12 = " (rat-str (rat-make 3 12)))
;(log "-1/5 = " (rat-str (rat-make -1 5)))
;(log "2/-3 = " (rat-str (rat-make 2 -3)))
;(log "-18/-36 = " (rat-str (rat-make -18 -36)))

(log "1/3 + 4/5 = " (rat-str (rat-add (rat-make 1 3) (rat-make 4 5))))
(log "4/5 - 1/3 = " (rat-str (rat-sub (rat-make 4 5) (rat-make 1 3))))
(log "7/10 * 2/3 = " (rat-str (rat-mul (rat-make 7 10) (rat-make 2 3))))
(log "7/10 ÷ 3/2 = " (rat-str (rat-div (rat-make 7 10) (rat-make 3 2))))
