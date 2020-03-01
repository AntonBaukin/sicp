; By including this file you enable debug mode by default.
;(define-value-if-not 'eval-debug-mode #t)

; Default implementation of the logging utility.
(define (eval-debug-log . args) (for-each display args) (newline))

(define-macro (eval-if-debug . script)
 `(if eval-debug-mode
   (begin ,@script)
  )
)
