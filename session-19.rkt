#lang racket

;; Lexical Address example
(lambda (f)
  ((lambda (h)
     (lambda (n)
       ((f (h h)) n)))
   (lambda (h)
     (lambda (n)
       ((f (h h)) n))))) 


; (f : 2 0)
; (h : 1 0) 
; (h : 1 0) 
; (n : 0 0) 

; (f : 2 0) 
; (h : 1 0) 
; (h : 1 0) 
; (n : 0 0)

