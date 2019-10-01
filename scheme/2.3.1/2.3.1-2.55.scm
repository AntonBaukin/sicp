(define (log . args) (for-each display args) (newline))

(log "'abracadabra == (quote abracadabra) ?= "
  (equal? 'abracadabra (quote abracadabra))
)

(log "''abracadabra == (quote (quote abracadabra)) ?= "
  (equal? ''abracadabra (quote (quote abracadabra)))
)

(log "(car (quote (quote abracadabra))) := "
  (car (quote (quote abracadabra)))
)

(log "(cadr (quote (quote abracadabra))) := "
  (cadr (quote (quote abracadabra)))
)
