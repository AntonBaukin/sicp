(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "stream.scm")
(include "interfaces.scm")
(include "defs.scm")
(include "streams-db.scm")
(include "assertions.scm")

(define adb (make-assertions-db make-streams-db))
(define adb-add (assertions-db-add adb))
(define adb-fetch (assertions-db-fetch adb))

(define Mike '(boy Mike))
(define John '(boy John))
(define Friend '(friend Mike John))

(assert-test Mike statement?)
(assert-test John statement?)
(assert-test Friend statement?)

(assert-test (make-assertion Mike) assertion?)
(assert-test (make-assertion John) assertion?)
(assert-test (make-assertion Friend) assertion?)

(define (add statement)
 (adb-add (make-assertion statement))
)

(add Mike)
(add John)
(add Friend)

(define frame (make-frame '()))
(assert-test frame frame?)

(define GetBoys '(boy (? . name)))
(define GetMikeFriends '(friend Mike (? . name)))
(define GetMikeInfo '((? . what) Mike))

(assert-test GetBoys query?)
(assert-test GetMikeFriends query?)
(assert-test GetMikeInfo query?)

(define (fetch query)
 (stream->list (adb-fetch (make-pattern query) frame))
)

(assert-equal?
 '((assertion boy John) (assertion boy Mike))
 (fetch GetBoys)
)

(assert-equal?
 '((assertion friend Mike John))
 (fetch GetMikeFriends)
)

(assert-equal?
 '((assertion boy John) (assertion boy Mike) (assertion friend Mike John))
 (fetch GetMikeInfo)
)
