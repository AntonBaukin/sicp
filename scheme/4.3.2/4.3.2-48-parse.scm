
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

 (global parse-simple-sentence)
 (global parse-sentence)
)
