;
; Vocabulary.
;
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(amb-eval-define 'nouns nouns)
(amb-eval-define 'verbs verbs)
(amb-eval-define 'articles articles)
(amb-eval-define 'prepositions prepositions)

;
; Implementation of parser from §4.3.2.
;
(eval-basic
 (define *unparsed* '())

 (define (parse input)
  (debug log "parse:> input = " input)
  (set! *unparsed* input)

  (let ((result (parse-sentence)))
   (require (null? *unparsed*))
   result
  )
 )

 (define (parse-sentence)
  (list 'sentence
   (parse-noun-phrase)
   (parse-verb-phrase)
  )
 )

 (define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
   (parse-word articles)
   (parse-word nouns)
  )
 )

 (define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
   (amb
    noun-phrase
    (maybe-extend
     (list
      'noun-phrase
      noun-phrase
      (parse-prepositional-phrase)
     )
    )
   )
  )

  (maybe-extend (parse-simple-noun-phrase))
 )

 (define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
   (amb
    verb-phrase
    (maybe-extend
     (list
      'verb-phrase
      verb-phrase
      (parse-prepositional-phrase)
     )
    )
   )
  )

  (maybe-extend (parse-word verbs))
 )

 (define (parse-prepositional-phrase)
  (list 'prep-phrase
   (parse-word prepositions)
   (parse-noun-phrase)
  )
 )

 (define (parse-word word-list)
  (debug log "parse:> word = «"
   (if (null? *unparsed*) "" (car *unparsed*)) "»"
   " is " (car word-list) "?"
  )

  (require-not (null? *unparsed*))
  (require (memq (car *unparsed*) (cdr word-list)))

  (let ((found-word (car *unparsed*)))
   (set! *unparsed* (cdr *unparsed*))
   (list (car word-list) found-word)
  )
 )

 (global parse)
)

; Logging helper.
(define (parse-and-log phrase)
 (define items '())
 (define output '())

 (define (spaces phrase)
  (append
   (list (car phrase))
   (if (null? (cdr phrase)) '() (append '(" ") (spaces (cdr phrase))))
  )
 )

 (amb-eval-define 'phrase phrase)
 (set! items (eval-amb-results (parse phrase)))

 (for-each
  (lambda (item)
   (set! output (append output (list ">> " item "\n")))
  )
  items
 )

 (apply log (append (list "—— ") (spaces phrase) (list "\n") output))
)
