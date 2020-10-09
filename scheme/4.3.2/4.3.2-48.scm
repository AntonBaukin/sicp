(define (log . args) (for-each display args) (newline))

(include "../4.3.1/eval-amb.scm")
(include "4.3.2-45-parse.scm")
(include "4.3.2-48-parse.scm")

; Turn on debugging to trace parsing:
; (eval-basic (debug on))

; Simple adjective:
(parse-and-log '(the white cat eats))
;
; >> (sentence
;  (noun-phrase (article the) (adjective white) (noun cat))
;  (verb eats)
; )

; Simple adjective and a verb with preposition:
(parse-and-log '(the white cat eats on the floor))
;
; >> (sentence
;  (noun-phrase (article the) (adjective white) (noun cat))
;  (verb-phrase
;   (verb eats)
;   (prep-phrase (prep on) (noun-phrase (article the) (noun floor)))
;  )
; )

; A next-to verb with adjective:
(parse-and-log '(the cat hastily eats))
;
; >> (sentence
;  (noun-phrase (article the) (noun cat))
;  (adverb-close (adverb hastily) (verb eats))
; )

; A next-to verb with adjective by the verb with preposition:
(parse-and-log '(the cat hastily eats on the floor))
;
; >> (sentence
;  (noun-phrase (article the) (noun cat))
;  (verb-phrase
;   (adverb-close (adverb hastily) (verb eats))
;   (prep-phrase (prep on) (noun-phrase (article the) (noun floor)))
;  )
; )

; Adjective in the propositional clause:
(parse-and-log '(the white cat hastily eats on the black floor))
;
; >> (sentence
;  (noun-phrase (article the) (adjective white) (noun cat))
;  (verb-phrase
;   (adverb-close (adverb hastily) (verb eats))
;   (prep-phrase
;    (prep on)
;    (noun-phrase (article the) (adjective black) (noun floor))
;   )
;  )
; )

; Trailing adverb in simple sentence:
(parse-and-log '(the cat eats hastily))
;
; >> (sentence
;  (noun-phrase (article the) (noun cat))
;  (adverb-phrase (verb eats) (adverb hastily))
; )

; Adjectives and the trailing adverb:
(parse-and-log '(the white cat eats on the black floor hastily))
;
; >> (sentence
;  (noun-phrase (article the) (adjective white) (noun cat))
;  (adverb-phrase
;   (verb-phrase
;    (verb eats)
;    (prep-phrase
;     (prep on)
;     (noun-phrase (article the) (adjective black) (noun floor))
;    )
;   )
;   (adverb hastily)
;  )
; )

; Compound sentence of two:
(parse-and-log '(the cat eats while the student sleeps))
;
; >> (compound-sentence
;  (sentence (noun-phrase (article the) (noun cat)) (verb eats))
;  (conjunction while)
;  (sentence (noun-phrase (article the) (noun student)) (verb sleeps))
; )

; Compound sentence of three:
(parse-and-log '(the cat eats and the student sleeps and the professor lectures))
;
; >> (compound-sentence
;  (sentence (noun-phrase (article the) (noun cat)) (verb eats))
;  (conjunction and)
;  (sentence (noun-phrase (article the) (noun student)) (verb sleeps))
;  (conjunction and)
;  (sentence (noun-phrase (article the) (noun professor)) (verb lectures)
;  )
; )

; Compound sentence of three filled with various stuff:
(parse-and-log '(
 the white cat eats hastily
 and the lazy student calmly sleeps by the black desk
 while the professor lectures to the class
))
;
; >> (compound-sentence
;  (sentence
;   (noun-phrase (article the) (adjective white) (noun cat))
;   (adverb-phrase (verb eats) (adverb hastily))
;  )
;  (conjunction and)
;  (sentence
;   (noun-phrase (article the) (adjective lazy) (noun student))
;   (verb-phrase
;    (adverb-close (adverb calmly) (verb sleeps))
;    (prep-phrase
;     (prep by)
;     (noun-phrase (article the) (adjective black) (noun desk))
;    )
;   )
;  )
;  (conjunction while)
;  (sentence
;   (noun-phrase (article the) (noun professor))
;   (verb-phrase
;    (verb lectures)
;    (prep-phrase
;     (prep to)
;     (noun-phrase (article the) (noun class))
;    )
;   )
;  )
; )
