(include "2.1.4-2.y.scm")

(log "interval [0.9 .. 1.1] is "
 (interval-str-% (interval-make 0.9 1.1))
)

(log "interval [1 ± 15%] is "
 (interval-str (interval-make-% 1 15))
)
