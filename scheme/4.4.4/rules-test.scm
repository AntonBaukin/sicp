(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "stream.scm")
(include "interfaces.scm")
(include "defs.scm")
(include "streams-db.scm")
(include "rules.scm")

(define rdb (make-rules-db make-streams-db))
(define rdb-add (rules-db-add rdb))
(define rdb-fetch (rules-db-fetch rdb))

(define Same2 '(rule (same (? . x) (? . x))))
(assert-test Same2 rule-correct?)
(rdb-add Same2)

(define Same3 '(rule (same (? . x) (? . x) (? . x))))
(assert-test Same3 rule-correct?)
(rdb-add Same3)

(define Ab '(rule (ab (? . a) (? . b)) (same (? . a) (? . b))))
(assert-test Ab rule-correct?)
(rdb-add Ab)

(define frame (make-frame '()))
(assert-test frame frame?)

(define (fetch query)
 (stream->list (rdb-fetch (make-pattern query) frame))
)

(define GetSame '(same (? . a) (? . b)))
(define GetAb '(ab (? . a) (? . b)))
(define GetVar '((? . rule) (? . a)))

(assert-equal?
 '(
   (rule (same (? . x) (? . x) (? . x)))
   (rule (same (? . x) (? . x)))
  )
 (fetch GetSame)
)

(assert-equal?
 '(
   (rule (ab (? . a) (? . b)) (same (? . a) (? . b)))
  )
 (fetch GetAb)
)

(assert-equal?
 '(
   (rule (ab (? . a) (? . b)) (same (? . a) (? . b)))
   (rule (same (? . x) (? . x) (? . x)))
   (rule (same (? . x) (? . x)))
  )
 (fetch GetVar)
)
