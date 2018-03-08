#lang racket

(define all-varrefs
  (lambda (exp)
    (cond ((varref? exp) (set exp))
          ((lambda? exp) (all-varrefs (lambda->body exp)))
          (else (set-union (all-varrefs (app->proc exp))
                           (all-varrefs (app->arg exp))))
          )
    ))