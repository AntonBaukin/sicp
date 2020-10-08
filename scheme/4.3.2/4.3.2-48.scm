(define (log . args) (for-each display args) (newline))

(include "../4.3.1/eval-amb.scm")
(include "4.3.2-45-parse.scm")

; Turn on debugging to trace parsing:
; (eval-basic (debug on))

(define adjectives '(adjective white black lazy))
(define adverbs '(adverb calmly hastily))
(define conjunctions '(conjunction and while))

(amb-eval-define 'adjectives adjectives)
(amb-eval-define 'adverbs adverbs)
(amb-eval-define 'conjunctions conjunctions)

;
; In this implementation we exclude tags such as «simple-noun-phrase»
; as there are too many combinations. The sub-type of a phrase may be
; found by checking it's content.
;
(eval-basic
 (define (parse-simple-noun-phrase)
  (amb
   (list 'noun-phrase
    (parse-word articles)
    (parse-word adjectives)
    (parse-word nouns)
   )
   (list 'noun-phrase
    (parse-word articles)
    (parse-word nouns)
   )
  )
 )

 (global parse-simple-noun-phrase)

 (define (parse-trailing-adverb verb-phrase)
  (list 'adverb-phrase verb-phrase (parse-word adverbs))
 )

 (define (adverb-extend verb-phrase)
  (amb
   verb-phrase
   (plog (parse-trailing-adverb verb-phrase) 'trailing-adverb)
  )
 )

 (define (parse-prep-verb verb-phrase)
  (list
   'verb-phrase
   verb-phrase
   (plog (parse-prepositional-phrase) 'prep-phrase)
  )
 )

 (define (verb-extend verb-phrase)
  (amb
   verb-phrase
   (verb-extend
    (parse-prep-verb verb-phrase)
   )
  )
 )

 (define (parse-close-adverb)
  (amb
   (parse-word verbs)
   (list 'adverb-close (parse-word adverbs) (parse-word verbs))
  )
 )

 (define (parse-verb-phrase)
  (adverb-extend
   (verb-extend
    (parse-close-adverb)
   )
  )
 )

 (global parse-verb-phrase)

 (define (parse-simple-sentence)
  (list 'sentence
   (plog (parse-noun-phrase) 'noun-phrase)
   (plog (parse-verb-phrase) 'verb-phrase)
  )
 )

 (define (parse-compound-sentence result)
  (amb
   result
   (parse-compound-sentence
    (append
     (if (eq? 'sentence (car result))
      (list 'compound-sentence result)
      result
     )
     (list
      (parse-word conjunctions)
      (parse-simple-sentence)
     )
    )
   )
  )
 )

 (define (parse-sentence)
  (parse-compound-sentence (parse-simple-sentence))
 )

 (global parse-sentence)
)

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
