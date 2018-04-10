#lang racket

(require "syntax-procs.rkt")

(define lexical-address
  (lambda (exp)
    (cond ((varref? exp)
           (list exp ': 0 0))
          ((lambda? exp)
           (cons (lexical-address (first (lambda->params exp))))
          ((app? exp)
           (map lexical-address exp))
          ((if? exp)
           (make-if
            (lexical-address (if->test exp))
            (lexical-address (if->then exp))
            (lexical-address (if->else exp))))
      )
    ))
