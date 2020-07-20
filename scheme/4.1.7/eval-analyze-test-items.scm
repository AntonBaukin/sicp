(eval-basic (debug on))

; Analyzing Evaluator: special form «try»:
(assert-eq? 1 (eval-basic (try 1)))
(assert-eq? void (eval-basic (try (/ 1 0))))
(assert-eq? 2 (eval-basic (try (/ 1 0) 2)))

(log "Analyzing Dispatch Evaluator §4.1.7 successfully tested!" "\n")
