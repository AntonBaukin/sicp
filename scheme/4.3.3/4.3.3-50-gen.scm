;
; With our single-argument list for «ramb»,
; we simply call it...
;
; We also apply limit of 8 words to the sentence.
;
(eval-basic
 (define (amb-of items)
  (require (not (null? items)))
  (require (> 8 (length (get-generated))))
  (ramb items)
 )

 (global amb-of)
)
