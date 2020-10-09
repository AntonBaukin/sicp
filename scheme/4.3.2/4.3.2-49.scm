(define (log . args) (for-each display args) (newline))

(include "../4.3.1/eval-amb.scm")
(include "4.3.2-45-parse.scm")
(include "4.3.2-48-parse.scm")
(include "4.3.2-49-gen.scm")

; Turn on debugging to trace the generation:
;(eval-basic (debug on))

(define (spaces phrase)
 (append
  (list (car phrase))
  (if (null? (cdr phrase)) '() (append '(" ") (spaces (cdr phrase))))
 )
)

(for-each
 (lambda (sentence)
  (apply log (append '(">> ") (spaces sentence) '("\n")))
 )
 (eval-amb-lim 10 (generate))
)
;
; >> the white student studies
;
; >> the white student studies calmly
;
; >> the white student studies hastily
;
; >> the white student studies for the white student
;
; >> the white student studies for the white student calmly
;
; >> the white student studies for the white student hastily
;
; >> the white student studies for the white student for the white student
;
; >> the white student studies for the white student for the white student calmly
;
; >> the white student studies for the white student for the white student hastily
;
; >> the white student studies for the white student for the white student
;    for the white student

;
; Then it goes deeply into for-clauses, as it's said in the task.
; Let's continue in task 50...
;
