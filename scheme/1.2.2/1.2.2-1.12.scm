(define (log . args) (for-each display args) (newline))

(define (pascal row column) (cond
  ((< row 0) 0)
  ((< column 0) 0)
  ((= column 0) 1)
  ((= column row) 1)
  ((> column row) 0)
  (else (+
    (pascal (- row 1) (- column 1))
    (pascal (- row 1) column)
  ))
))

(log "    " (pascal 0 0))
(log "   " (pascal 1 0) " " (pascal 1 1) )
(log "  " (pascal 2 0) " " (pascal 2 1) " " (pascal 2 2))
(log " " (pascal 3 0) " " (pascal 3 1) " " (pascal 3 2) " " (pascal 3 3))
(log (pascal 4 0) " " (pascal 4 1) " " (pascal 4 2) " " (pascal 4 3) " " (pascal 4 4))
