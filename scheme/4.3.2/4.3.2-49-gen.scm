
(eval-basic
 (define *generated* '())
 (define (get-generated) (reverse *generated*))

 (define (reset-generated)
  (set! *generated* '())
 )

 (define (amb-of items)
  (require (not (null? items)))
  (amb (car items) (amb-of (cdr items)))
 )

 (define (parse-word word-list)
  (define word (amb-of (cdr word-list)))
  (debug log "gen:> word = «" word "»")
  (set! *generated* (cons word *generated*))
 )

 ; We exclude compound sentences from the generation
 ; as they tend to grow rapidly in length.
 (define (parse-sentence)
  (parse-simple-sentence)
 )

 (define (generate)
  (reset-generated)
  (parse-sentence)
  (get-generated)
 )

 (global get-generated)
 (global reset-generated)
 (global parse-word)
 (global generate)
)
