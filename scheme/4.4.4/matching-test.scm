(define (log . args) (for-each display args) (newline))

(include "../3.3.2/assert.scm")
(include "defs.scm")
(include "utilities.scm")
(include "matching.scm")

(define (match query statement)
 (define frame (pattern-match (parse-query query) statement empty-frame))
 (if (eq? void frame) 'null (frame->list frame))
)

; Matching: dummy queries without variables.
(assert-equal? '() (match '(a) '(a)))
(assert-equal? '() (match '(a b) '(a b)))
(assert-equal? '() (match '(a b c) '(a b c)))
(assert-equal? '() (match '(a (b)) '(a (b))))
(assert-equal? '() (match '(a (b) c) '(a (b) c)))
(assert-equal? '() (match '(a (b c)) '(a (b c))))
(assert-equal? '() (match '(a (b c) d) '(a (b c) d)))

; Matching: not matching dummy queries without variables.
(assert-equal? 'null (match '(a) '(b)))
(assert-equal? 'null (match '(a) '(a b)))
(assert-equal? 'null (match '(a c) '(a b)))
(assert-equal? 'null (match '(a b c) '(a b d)))
(assert-equal? 'null (match '(a (b) c) '(a b c)))
(assert-equal? 'null (match '(a (b c) d) '(a (b d) d)))

; Matching: single variable of plain value.
(assert-equal? '((a a)) (match '(?a) '(a)))
(assert-equal? '((a a)) (match '(?a b) '(a b)))
(assert-equal? '((b b)) (match '(a ?b) '(a b)))
(assert-equal? '((b b)) (match '(a ?b c) '(a b c)))

; Matching: multiple variables of plain values.
(assert-equal? '((b b) (a a)) (match '(?a ?b) '(a b)))
(assert-equal? '((c c) (b b) (a a)) (match '(?a ?b ?c) '(a b c)))
(assert-equal? '((b b) (a a)) (match '(x ?a ?b y) '(x a b y)))

; Matching: variables of list values.
(assert-equal? '((ab (a b))) (match '(?ab) '((a b))))
(assert-equal? '((ab (a b)) (x x)) (match '(?x ?ab) '(x (a b))))
(assert-equal? '((cde (c d e)) (ab (a b))) (match '(?ab ?cde) '((a b) (c d e))))
(assert-equal? '((de (d e)) (c (c)) (ab (a b))) (match '(?ab ?c ?de) '((a b) (c) (d e))))

; Matching: nested variables.
(assert-equal? '((a a)) (match '((?a)) '((a))))
(assert-equal? '((b b) (a a)) (match '((?a (?b))) '((a (b)))))

; Matching: matching constistency.
(assert-equal? '((x (a b))) (match '(?x c ?x) '((a b) c (a b))))
(assert-equal? 'null (match '(?x c ?x) '((a b) b (a b))))
(assert-equal? 'null (match '(?x c ?x) '((a b) c (a d))))

; Matching: dot notation.
(assert-equal? '((b (b)) (a a)) (match '(?a . ?b) '(a b)))
(assert-equal? '((cd (c d)) (a a)) (match '(?a b . ?cd) '(a b c d)))
(assert-equal? '((cd (c d)) (b b) (a a)) (match '(?a (?b . ?cd)) '(a (b c d))))
