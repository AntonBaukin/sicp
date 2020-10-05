(define (log . args) (for-each display args) (newline))

(include "../4.3.1/eval-amb.scm")
(include "4.3.2-45-parse.scm")

(eval-basic (debug on))

(parse-and-log '(the student with the cat sleeps in the class))

; So, implement Hugo's proposal...
(eval-basic
 (define (parse-noun-phrase)
  (amb
   (parse-simple-noun-phrase)
   (list
    'verb-phrase
    (parse-noun-phrase)
    (parse-prepositional-phrase)
   )
  )
 )

 (define (parse-verb-phrase)
  (amb
   (parse-word verbs)
   (list
    'verb-phrase
    (parse-verb-phrase)
    (parse-prepositional-phrase)
   )
  )
 )

 (global parse-noun-phrase)
 (global parse-verb-phrase)
)

(log "\n" "—— And now comes Hugo's version..." "\n\n")

; And taste Hugo's version:
(parse-and-log '(the student with the cat sleeps in the class))
;
; With our special support for parser limiting, we get:
;
; parse:> word = «the» is article?
; parse:> word = «student» is noun?
; parse:> noun-phrase = (simple-noun-phrase (article the) (noun student))
; parse:> word = «with» is verb?
; ...
; *** ERROR IN apply-impl, ... -- Parse seems hung on: with
;
; This is because when «parse-verb-phrase» fails on the first
; amb option (testing «with» being a verb), it goes to the
; second option and does the same test recursively.
;

; So, lets swap amb options...
(eval-basic
 (define (parse-noun-phrase)
  (amb
   (list
    'verb-phrase
    (parse-noun-phrase)
    (parse-prepositional-phrase)
   )
   (parse-simple-noun-phrase)
  )
 )

 (define (parse-verb-phrase)
  (amb
   (list
    'verb-phrase
    (parse-verb-phrase)
    (parse-prepositional-phrase)
   )
   (parse-word verbs)
  )
 )

 (global parse-noun-phrase)
 (global parse-verb-phrase)
)
;
; And taste it again:
; (parse-and-log '(the student with the cat sleeps in the class))
;
; It never stops as it goes directly in infinite recursion...
;
