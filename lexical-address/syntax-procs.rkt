;;  ------------------------------------------------------------------------
;; |   FILE           :  syntax-procs.rkt                                   |
;; |   AUTHOR         :  Eugene Wallingford                                 |
;; |   CREATION DATE  :  2018/02/18                                         |
;; |   DESCRIPTION    :  These functions implement syntax procedures for    |
;; |                     a simple language grammar consisting only of       |
;; |                     variable references, lambda expressions, and       |
;; |                     function applications.                             |
;;  ------------------------------------------------------------------------
;; |   MODIFIED       :  2018/03/01  Eugene Wallingford                     |
;; |   DESCRIPTION    :  Added a let expression to the little language.     |
;;  ------------------------------------------------------------------------
;; |   MODIFIED       :  2018/03/02  Eugene Wallingford                     |
;; |   DESCRIPTION    :  Added an if expression to the little language.     |
;;  ------------------------------------------------------------------------
;; |   MODIFIED       :  2018/03/08  Zachary Wright                         |
;; |   DESCRIPTION    :  Added specifications for homework 07               |
;;  ------------------------------------------------------------------------

#lang racket
(provide exp?
         varref? make-varref
         bool?   make-bool
         lambda? lambda->param lambda->body make-lambda
         app?    app->proc     app->arg    make-app
         if?     if->test      if->then    if->else   make-if       ; <--- NEW FEATURE
         let?    let->var      let->val    let->body  make-let
         or?     or->arg1      or->arg2    make-or
         and?    and->arg1     and->arg2   make-and)

;;  ------------------------------------------------------------------------
;;   This code works with the following grammar:
;;
;;        <exp>      ::= <varref>
;;                     | ( lambda ( <var> ) <exp> )
;;                     | ( <exp> <exp> )
;;                     | ( if <exp> <exp> <exp> )           <--- NEW FEATURE
;;                     | ( let (<exp> <exp>) <exp> )
;;  ------------------------------------------------------------------------


;;  ------------------------------------------------------------------------
;;  general type predicate

(define exp?
  (lambda (exp)
    (or (varref? exp)
        (bool?   exp)
        (lambda? exp)
        (app?    exp)
        (if?     exp)           ; <--- NEW FEATURE
        (let?    exp))))

;;  ------------------------------------------------------------------------
;;  varrefs

(define varref?
  (lambda (exp)
    (and (not (eq? 'true exp))
         (not (eq? 'false exp))
         (symbol? exp))))

;; constructor added
(define make-varref
  (lambda (sym)
    sym
    ))

;;  ------------------------------------------------------------------------
;;  booleans
(define bool?
  (lambda (exp)
    (or (eq? 'false exp)
        (eq? 'true exp))))

(define make-bool
  (lambda (sym)
    sym
    ))
;;  ------------------------------------------------------------------------
;;  lambda expressions

(define lambda?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 3)
         (eq? (car exp) 'lambda)
         (varref? (caadr exp))
         (exp? (third exp)))))

(define lambda->param caadr)
(define lambda->body  third)

(define make-lambda
  (lambda (arg body)
    (list 'lambda (list arg) body) 
    ))

;;  ------------------------------------------------------------------------
;;  application expressions  ("apps")

(define app?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 2)
         (exp? (first exp))
         (exp? (second exp)))))

(define app->proc first)
(define app->arg  second)

(define make-app
  (lambda (func arg)
    (list func arg)
    ))

;;  ------------------------------------------------------------------------
;;  if expressions

(define if?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 4)
         (eq?  (first  exp) 'if)
         (exp? (second exp))
         (exp? (third  exp))
         (exp? (fourth exp)))))

(define if->test second)
(define if->then third)
(define if->else fourth)

(define make-if
  (lambda (test then else)
    (list 'if test then else)
    ))

;;  ------------------------------------------------------------------------
;;  let expressions

(define let?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 3)
         (eq? 'let (first exp))
         (binding? (second exp))
         (exp? (third exp)))))

(define binding?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 2)
         (varref? (first exp))
         (exp? (second exp)))))

(define let->var
  (lambda (let-exp)
    (first (second let-exp))))

(define let->val
  (lambda (let-exp)
    (second (second let-exp))))

(define let->body third)

(define make-let
  (lambda (var val body)
    (list 'let (list var val) body)
    ))
;;  ------------------------------------------------------------------------
;;  or expressions
(define or?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 3)
         (eq? 'or (first exp))
         (exp? (second exp))
         (exp? (third exp)))
    ))

(define or->arg1 second)
(define or->arg2 third)

(define make-or
  (lambda (arg1 arg2)
    (list 'or arg1 arg2)
    ))
;;  ------------------------------------------------------------------------
;;  and expressions
(define and?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 3)
         (eq? 'and (first exp))
         (exp? (second exp))
         (exp? (third exp))
         )
    ))

(define and->arg1 second)
(define and->arg2 third)

(define make-and
  (lambda (arg1 arg2)
    (list 'and arg1 arg2)
    ))
;;  ------------------------------------------------------------------------