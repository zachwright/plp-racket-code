#lang racket

(define all-varrefs
  (lambda (exp)
    (cond ((varref? exp) (set exp))
          ((lambda? exp) (all-varrefs (lambda->body exp)))
          (else (set-union (all-varrefs (app->proc exp))
                           (all-varrefs (app->arg exp))))
          )
    ))

;; scope example in racket
(lambda (x y)
  ((lambda (a)
     (x (a y)))
   x))

; (x: 0 0)
; (x: 1 0)
; (y: 1 1)
; (a: 0 0)


;; LEXICAL ADDRESS
;; (v : d p)
;; v is a variable
;; d is depth of the varref from it's declaration
;; p is the position of variable in declaration list

