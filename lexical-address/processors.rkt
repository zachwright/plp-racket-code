;;  ------------------------------------------------------------------------
;; |   FILE           :  processors.rkt                                     |
;; |   AUTHOR         :  Eugene Wallingford
;; |   EDITED BY      :  Zachary Wright
;; |   CREATION DATE  :  2018/03/02                                         |
;; |   UPDATED ON     :  2018/03/06                                         |
;; |   DESCRIPTION    :  A collection of the functions we have written to   |
;; |                     process programs written in our little language.   |
;; |                     They have been extended to handle if expressions.  |
;;  ------------------------------------------------------------------------
;; |   MODIFIED       :  2018/03/08  Zachary Wright                         |
;; |   DESCRIPTION    :  Added specifications for homework 07               |
;;  ------------------------------------------------------------------------

#lang racket
(require "syntax-procs.rkt")
(provide (all-defined-out))    ; this exports all function definitions

;;  ------------------------------------------------------------------------
;;   This code works with the following grammar:
;;
;;        <exp>      ::= <varref>
;;                     | ( lambda ( <var> ) <exp> )
;;                     | ( <exp> <exp> )
;;                     | ( if <exp> <exp> <exp> )           <--- NEW FEATURE
;;                     | ( let (<exp> <exp>) <exp> )
;;  ------------------------------------------------------------------------

;; -------------------------------------------------------------------------
;; This function preprocesses programs from the full language, translating
;; syntactic sugar into core features of LL.
;; -------------------------------------------------------------------------

(define preprocess
  (lambda (exp)
    (cond ;; core features --------------------------------------
      ((varref? exp) (make-varref exp))
      ((bool? exp) (make-bool exp))
      ((lambda? exp) (make-lambda (lambda->param exp)
                                  (preprocess (lambda->body exp))) )
      ((app? exp)    (map preprocess exp) )
      ((if? exp)     (make-if  (preprocess (if->test exp))
                               (preprocess (if->then exp))
                               (preprocess (if->else exp))) )
      ;; syntactic sugar ------------------------------------
      ((or? exp)     (let [(arg1 (or->arg1 exp))
                           (arg2 (or->arg2 exp))]
                           (make-if (preprocess arg1)
                                    (preprocess 'true)
                                    (preprocess arg2)) ))
      ((and? exp)    (let [(arg1 (and->arg1 exp))
                           (arg2 (and->arg2 exp))]
                           (make-if (preprocess arg1)
                                    (preprocess arg2)
                                    (preprocess 'false) )))    
    ;; let?
    (else          (let ((var  (let->var  exp))
                         (val  (let->val  exp))
                         (body (let->body exp)))
                     (make-app (make-lambda var
                                            (preprocess body))
                               (preprocess val)) ) ))))

;; -------------------------------------------------------------------------
;; These functions do static analysis on programs in core LL.
;; -------------------------------------------------------------------------

(define free-vars
  (lambda (exp)
    (cond ((varref? exp) '())
          ((lambda? exp) (cons (lambda->param exp)
                               (free-vars (lambda->body exp))))
          ((if? exp)     (append (free-vars (if->test exp))
                                 (free-vars (if->then exp))
                                 (free-vars (if->else exp))))
        
        )))



(define occurs-bound?
  (lambda (s exp)
    (cond ((varref? exp) #f)
          ((app? exp)    (or (occurs-bound? s (app->proc exp))
                             (occurs-bound? s (app->arg  exp))))
          ((if? exp)     (or (occurs-bound? s (if->test exp))
                             (occurs-bound? s (if->then exp))
                             (occurs-bound? s (if->else exp))))
          ;; lambda?
          (else          (or (occurs-bound? s (lambda->body exp))
                             (and (eq? s (lambda->param exp))
                                  (occurs-free? s (lambda->body exp))))))))

(define occurs-free?
  (lambda (s exp)
    (cond ((varref? exp) (eq? s exp))
          ((app? exp)    (or (occurs-free? s (app->proc exp))
                             (occurs-free? s (app->arg  exp))))
          ((if? exp)     (or (occurs-free? s (if->test exp))
                             (occurs-free? s (if->then exp))
                             (occurs-free? s (if->else exp))))
          ;; lambda?
          (else          (and (not (eq? s (lambda->param exp)))
                              (occurs-free? s (lambda->body exp)))))))

(define declared-vars
  (lambda (exp)
    (cond ((varref? exp) '())
          ((lambda? exp) (cons (lambda->param exp)
                               (declared-vars (lambda->body exp))))
          ((if? exp)     (append (declared-vars (if->test exp))
                                 (declared-vars (if->then exp))
                                 (declared-vars (if->else exp))))
          ;; app?
          (else          (append (declared-vars (app->proc exp))
                                 (declared-vars (app->arg  exp)))))))

(define is-declared?
  (lambda (v exp)
    (cond ((varref? exp) #f )
          ((app? exp)    (or (is-declared? v (app->proc exp))
                             (is-declared? v (app->arg  exp))) )
          ((if? exp)     (or (is-declared? v (if->test exp))
                             (is-declared? v (if->then exp))
                             (is-declared? v (if->else exp))) )
          ;; lambda?
          (else          (or (eq? v (lambda->param exp))
                             (is-declared? v (lambda->body exp))) ))))

;; -------------------------------------------------------------------------
