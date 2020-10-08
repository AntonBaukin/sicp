;
; Vocabulary.
;
(define nouns '(noun student professor cat class floor desk))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with of on))

(amb-eval-define 'nouns nouns)
(amb-eval-define 'verbs verbs)
(amb-eval-define 'articles articles)
(amb-eval-define 'prepositions prepositions)

; Counter without amb unset.
(define (make-counter value)
 (lambda (m . args)
  (cond
   ((eq? m 'get) value)
   ((eq? m 'set) (set! value (car args)))
   ((eq? m 'inc) (set! value (+ value 1)))
  )
 )
)

(amb-eval-define 'make-counter make-counter)

;
; Implementation of parser from §4.3.2.
;
(eval-basic
 (define LIMIT 10)
 (define counter (make-counter 0))
 (define *unparsed* '())

 (define (get-parsed) *unparsed*)

 (define (set-parsed input)
  (debug log "parse:> input = " input)
  (set! *unparsed* input)
  (counter 'set 0)
 )

 (define (parse-word word-list)
  (require-not (null? *unparsed*))

  (if (> (counter 'get) LIMIT)
   (error "Parse seems hung on:" (car *unparsed*))
  )
  (counter 'inc)

  (debug log "parse:> word = «"
   (if (null? *unparsed*) "" (car *unparsed*)) "»"
   " is " (car word-list) "?"
  )

  (require (memq (car *unparsed*) (cdr word-list)))

  (let ((found-word (car *unparsed*)))
   (counter 'set 0)
   (set! *unparsed* (cdr *unparsed*))
   (list (car word-list) found-word)
  )
 )

 (define (plog result what)
  (debug log "parse:> " what " = " result)
  result
 )

 (global get-parsed)
 (global set-parsed)
 (global parse-word)
 (global plog)
)

(eval-basic
 (define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
   (parse-word articles)
   (parse-word nouns)
  )
 )

 (define (parse-prepositional-phrase)
  (list 'prep-phrase
   (parse-word prepositions)
   (parse-noun-phrase)
  )
 )

 (global parse-simple-noun-phrase)
 (global parse-prepositional-phrase)
)

(eval-basic
 (define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
   (amb
    noun-phrase
    (maybe-extend
     (list
      'noun-phrase
      noun-phrase
      (plog (parse-prepositional-phrase) 'prep-phrase)
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
      (plog (parse-prepositional-phrase) 'prep-phrase)
     )
    )
   )
  )

  (maybe-extend (parse-word verbs))
 )

 (global parse-noun-phrase)
 (global parse-verb-phrase)
)

(eval-basic
 (define (parse-sentence)
  (list 'sentence
   (plog (parse-noun-phrase) 'noun-phrase)
   (plog (parse-verb-phrase) 'verb-phrase)
  )
 )

 (global parse-sentence)
)

(eval-basic
 (define (parse input)
  (set-parsed input)

  (let ((result (parse-sentence)))
   (require (null? (get-parsed)))
   result
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
